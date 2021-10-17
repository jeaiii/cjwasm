using uint8_t = unsigned char;
using uint16_t = unsigned short;
using uint32_t = unsigned int;
using int8_t = signed char;
using int16_t = short;
using int32_t = int;
using uint64_t = unsigned long long;
using int64_t = long long;

namespace cjwasm
{
    struct wasm
    {
        enum : uint8_t
        {
            op_unreachable, op_nop, op_block, op_loop, op_if, op_else,
            op_end = 0x0b, op_br, op_br_if, op_return = 0x0f,
            op_call = 0x10,

            op_drop = 0x1a, op_select,

            op_local_get = 0x20, op_local_set, op_local_tee,

            op_const_i32 = 0x41,
            op_const_i64,

            op_eqz_i32 = 0x45,
            op_eq_i32,
            op_ne_i32,
            op_lt_s_i32,
            op_lt_u_i32,
            op_gt_s_i32,
            op_gt_u_i32,
            op_le_s_i32,
            op_le_u_i32,
            op_ge_s_i32,
            op_ge_u_i32,

            op_add_i32 = 0x6a, op_sub_i32, op_mul_i32, op_div_s_i32, op_div_u_i32, op_rem_s_i32, op_rem_u_i32,
            op_and_i32, op_or_i32, op_xor_i32,

            bt_void = 0x40,
            bt_i32 = 0x7f, bt_i64 = 0x7e, bt_f32 = 0x7d, bt_f64 = 0x7c,
        };
    };

    union value_t
    {
        int32_t i32;
        uint32_t u32;
        int64_t i64;
        uint64_t u64;
        float f32;
        double f64;
        void (*code)(value_t const*, value_t*);
        value_t const* ip;
        value_t* sp;
    };

    using sp_t = value_t*;
    using ip_t = value_t const*;

    using code_t = value_t;

    void trap(ip_t, sp_t sp) { }

    template<class T, size_t N = 128, class...Ts> T call(ip_t ip, Ts...ts)
    {
        value_t stack[N];

        size_t i = 0;
        int _[]{ ((Ts&)stack[i++] = ts, 0)... };

        value_t done;
        done.code = trap;
        stack[sizeof...(Ts) + 0].ip = &done;
        stack[sizeof...(Ts) + 1].sp = nullptr;
        ip->code(ip + 1, stack);
        return (T const&)stack[0];
    }

    struct function
    {
        ip_t ip;
        int argument_count;
        int return_count;
    };

    struct compiler
    {

        uint8_t const* src_begin;
        uint8_t const* src;
        uint8_t const* src_end;

        code_t* dst_begin;
        code_t* dst;
        code_t* dst_end;

        function* functions;
        function* self;

        struct block
        {
            uint8_t op;
            uint8_t type;
            code_t* enter;
            code_t* leave;
        };

        block blocks[16];
        block* bp;

        uint8_t get_code() { return *src++; }
        uint8_t get_u8() { return *src++; }

        uint32_t get_leb128_u32()
        {
            // TODO https://en.wikipedia.org/wiki/LEB128
            return *src++;
        }

        uint32_t get_leb128_i32()
        {
            // TODO https://en.wikipedia.org/wiki/LEB128
            return uint32_t(int32_t(int8_t(*src++)));
        }


        uint64_t get_leb128_u64()
        {
            // TODO https://en.wikipedia.org/wiki/LEB128
            return *src++;
        }

        static uint32_t operand_u32(ip_t ip) { return ip->u32; }

        void emit(uint32_t a) { dst[0].u32 = a; ++dst; }
        void emit(uint64_t a) { dst[0].u64 = a; ++dst; }
        void emit(void (*f)(ip_t ip, sp_t sp)) { dst[0].code = f; ++dst; }
        void emit(void (*f)(ip_t ip, sp_t sp), uint32_t a) { emit(f), emit(a); }
        void emit(void (*f)(ip_t ip, sp_t sp), uint64_t a) { emit(f), emit(a); }
        void emit(void (*f)(ip_t ip, sp_t sp), decltype("" - "") a) { emit(f), emit(uint32_t(a)); }

        // unconditional branches don't depend on stack depth so handle them outside compile so there aren't unique ones per stack depth
        void do_branch(block& scope)
        {
            if (scope.op == wasm::op_loop)
            {
                // branch back
                emit([](ip_t ip, sp_t sp) { auto offset = operand_u32(ip); (ip - offset + 1)->code(ip - offset + 2, sp); }, dst - scope.enter + 2);
            }
            else
            {
                // branch forward
                emit([](ip_t ip, sp_t sp) { auto offset = operand_u32(ip); (ip + offset + 1)->code(ip + offset + 2, sp); }, dst - scope.leave);
                scope.leave = dst - 1;
            }
        }

        template<int N>
        int compile(int n)
        {
            auto do_forward_if = [](ip_t ip, sp_t sp) { auto offset = sp[N].i32 == 0 ? 0 : operand_u32(ip); (ip + offset + 1)->code(ip + offset + 2, sp); };
            auto emit_return = [this]
            {
                emit([](ip_t ip, sp_t sp)
                    {
                        auto return_ip = sp[N - 2].ip;
                        auto return_sp = sp[N - 1].sp;
                        auto n = ip[0].u32;
                        for (uint32_t i = 0; i < n; ++i)
                            sp[i] = sp[N - n + i + 1];

                        return_ip->code(return_ip + 1, return_sp);
                    }, uint32_t(self->return_count));
            };

            while (n == N) switch (get_code())
            {
            case wasm::op_nop:
                continue;

            case wasm::op_block:
                *--bp = { wasm::op_block, get_u8(), dst, nullptr };
                continue;
            case wasm::op_loop:
                *--bp = { wasm::op_loop, get_u8(), dst, nullptr };
                continue;
            case wasm::op_if:
                *--bp = { wasm::op_if, get_u8(), dst, nullptr };
                emit(do_forward_if, dst - dst);
                return N - 1;

            case wasm::op_else:
                // fix if branch
                (uint32_t&)bp[0].enter[1] = uint32_t(dst - bp[0].enter);
                // branch to leave
                do_branch(bp[0]);
                continue;

            case wasm::op_end:
                // fix up forward branches
                for (ip_t np = bp[0].leave; np != 0;)
                {
                    auto offset = (uint32_t&)np[0];
                    (uint32_t&)np[0] = uint32_t(dst - np) + 1;
                    np -= offset;
                    np = 0;
                }
                
                if (bp == &blocks[15])
                {
                    emit_return();
                    return 0;
                }
                ++bp;
                continue;

            case wasm::op_br:
                do_branch(bp[get_leb128_u32()]);
                continue;

            case wasm::op_br_if:
            {
                auto& scope = bp[get_leb128_u32()];
                if (scope.op == wasm::op_loop)
                {
                    // branch back
                    emit([](ip_t ip, sp_t sp) { auto offset = sp[N].i32 == 0 ? 0 : operand_u32(ip); (ip - offset + 1)->code(ip - offset + 2, sp); }, dst - scope.enter + 2);
                }
                else
                {
                    emit(do_forward_if, dst - scope.leave);
                    scope.leave = dst - 1;
                }
                return N - 1;
            }

            case wasm::op_return:
                emit_return();
                continue;
            case wasm::op_call:
            {
                auto const& f = functions[get_leb128_u32()];
                emit([](ip_t ip, sp_t sp)
                    {
                        sp[N + 1].ip = ip + 2;
                        sp[N + 2].sp = sp;

                        auto n = ip[0].u32;
                        ip = ip[1].ip;
                        ip->code(ip + 1, sp + N - n);
                    });
                dst[0].u32 = f.argument_count;
                dst[1].ip = f.ip;
                dst += 2;
                n += f.return_count - f.argument_count;
                continue;
            }
            case wasm::op_drop: return N - 1;
            case wasm::op_select:
                emit([](ip_t ip, sp_t sp) { if (sp[N].i32 != 0) sp[N - 2] = sp[N - 1]; ip->code(ip + 1, sp); });
                return N - 2;
            case wasm::op_local_get:
                emit([](ip_t ip, sp_t sp) { sp[N + 1] = sp[operand_u32(ip)]; (ip + 1)->code(ip + 2, sp); }, get_leb128_u32());
                return compile<N + 1>(N + 1);
            case wasm::op_local_set:
                emit([](ip_t ip, sp_t sp) { sp[operand_u32(ip)] = sp[N]; (ip + 1)->code(ip + 2, sp); }, get_leb128_u32());
                return N - 1;
            case wasm::op_local_tee:
                emit([](ip_t ip, sp_t sp) { sp[operand_u32(ip)] = sp[N]; (ip + 1)->code(ip + 2, sp); }, get_leb128_u32());
                continue;

            case wasm::op_const_i32:
                emit([](ip_t ip, sp_t sp) { sp[N + 1].i32 = ip->i32; (ip + 1)->code(ip + 2, sp); }, get_leb128_i32());
                return compile<N + 1>(N + 1);
            case wasm::op_const_i64:
                emit([](ip_t ip, sp_t sp) { sp[N + 1].i64 = ip->i64; (ip + 1)->code(ip + 2, sp); }, get_leb128_u64());
                return compile<N + 1>(N + 1);

            case wasm::op_eqz_i32: emit([](ip_t ip, sp_t sp) { sp[N].u32 = sp[N].u32 == 0 ? 1 : 0; ip->code(ip + 1, sp); }); continue;

            case wasm::op_eq_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 == sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_ne_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 != sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;

            case wasm::op_lt_s_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 < sp[N].i32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_lt_u_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 < sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_gt_s_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 > sp[N].i32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_gt_u_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 > sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_le_s_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 <= sp[N].i32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_le_u_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 <= sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_ge_s_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 >= sp[N].i32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_ge_u_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 >= sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;

            case wasm::op_add_i32:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 + sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_sub_i32:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 - sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_mul_i32:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 * sp[N].u32; ip->code(ip + 1, sp); }), N - 1;           
            case wasm::op_div_s_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 / sp[N].i32; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_div_u_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 / sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_rem_s_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 % sp[N].i32; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_rem_u_i32: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 % sp[N].u32; ip->code(ip + 1, sp); }), N - 1;

            case wasm::op_and_i32:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 & sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_or_i32:    return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 | sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case wasm::op_xor_i32:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 ^ sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            }
            while (n > N)
                n = compile<N + 1>(n);
            return n;
        }
        // compile error
        template<> int compile<8>(int) { return 7; }
        template<> int compile<1>(int) { return 0; }

        size_t compile_function(int n, size_t src_size, uint8_t const src_data[], size_t dst_size, code_t dst_data[])
        {
            src_begin = src_data;
            src = src_begin;
            src_end = src + src_size;

            dst_begin = dst_data;
            dst = dst_begin;
            dst_end = dst + dst_size;

            bp = &blocks[15];
            *bp = { wasm::op_block, wasm::bt_i32, dst, nullptr };

            function fs[8];
            functions = fs;

            functions[0] = { dst, n, 1 };
            self = &functions[0];

            compile<2>(1 + n);
            return dst - dst_begin;
        }
    };
}
