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
    extern "C" unsigned int __lzcnt(unsigned int);
    extern "C" unsigned int _tzcnt_u32(unsigned int);
    extern "C" unsigned int __popcnt(unsigned int);
    extern "C" unsigned int _rotl(unsigned int, int);
    extern "C" unsigned int _rotr(unsigned int, int);

    enum : uint8_t
    {
        op_unreachable, op_nop, op_block, op_loop, op_if, op_else, _op_06, _op_07, _op_08, _op_09, _op_0a, op_end, op_br, op_br_if, _op_br_table, op_return,
        
        op_call, _op_call_indirect, _op_return_call, _op_return_call_indirect, _op_14, _op_15, _op_16, _op_17, _op_18, _op_19, op_drop, op_select, _op_1c, _op_1d, _op_1e, _op_1f,

        op_local_get, op_local_set, op_local_tee, _op_global_get, _op_global_set, _op_table_get, _op_table_set, _op_27,
        
        _op_i32_load, _op_i64_load, _op_f32_load, _op_f64_load,
        _op_i32_load8_s, _op_i32_load8_u, _op_i32_load16_s, _op_i32_load16_u, _op_i64_load8_s, _op_i64_load8_u, _op_i64_load16_s, _op_i64_load16_u, _op_i64_load32_s, _op_i64_load32_u,

        _op_i32_store, _op_i64_store, _op_f32_store, _op_f64_store,
        _op_i32_store8, _op_i32_store16, _op_i64_store8, _op_i64_store16, _op_i64_store32,
        
        _op_memory_size,
        _op_memory_grow,

        op_i32_const, op_i64_const, _op_f32_const, _op_f64_const,

        op_i32_eqz, op_i32_eq, op_i32_ne, op_i32_lt_s, op_i32_lt_u, op_i32_gt_s, op_i32_gt_u, op_i32_le_s, op_i32_le_u, op_i32_ge_s, op_i32_ge_u,
        op_i64_eqz, op_i64_eq, op_i64_ne, op_i64_lt_s, op_i64_lt_u, op_i64_gt_s, op_i64_gt_u, op_i64_le_s, op_i64_le_u, op_i64_ge_s, op_i64_ge_u,

        _op_f32_eq, _op_f32_ne, _op_f32_lt, _op_f32_gt, _op_f32_le, _op_f32_ge,
        _op_f64_eq, _op_f64_ne, _op_f64_lt, _op_f64_gt, _op_f64_le, _op_f64_ge,

        op_i32_clz, op_i32_ctz, op_i32_popcnt,
        op_i32_add, op_i32_sub, op_i32_mul, op_i32_div_s, op_i32_div_u, op_i32_rem_s, op_i32_rem_u,
        op_i32_and, op_i32_or, op_i32_xor, op_i32_shl, op_i32_shr_s, op_i32_shr_u, op_i32_rotl, op_i32_rotr,

        _op_i64_clz, _op_i64_ctz, _op_i64_popcnt,
        _op_i64_add, _op_i64_sub, _op_i64_mul, _op_i64_div_s, _op_i64_div_u, _op_i64_rem_s, _op_i64_rem_u,
        _op_i64_and, _op_i64_or, _op_i64_xor, _op_i64_shl, _op_i64_shr_s, _op_i64_shr_u, _op_i64_rotl, _op_i64_rotr,

        _op_f32_abs, _op_f32_neg, _op_f32_ceil, _op_f32_floor, _op_f32_trunc, _op_f32_nearest, _op_f32_sqrt, _op_f32_add, _op_f32_sub, _op_f32_mul, _op_f32_div, _op_f32_min, _op_f32_max, _op_f32_copysign,
        _op_f64_abs, _op_f64_neg, _op_f64_ceil, _op_f64_floor, _op_f64_trunc, _op_f64_nearest, _op_f64_sqrt, _op_f64_add, _op_f64_sub, _op_f64_mul, _op_f64_div, _op_f64_min, _op_f64_max, _op_f64_copysign,


        bt_void = 0x40,
        bt_i32 = 0x7f, bt_i64 = 0x7e, bt_f32 = 0x7d, bt_f64 = 0x7c,
    };

    union value_t
    {
        int32_t i32;
        uint32_t u32;
        int64_t i64;
        uint64_t u64;
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

        uint8_t get_code() { return src < src_end ? *src++ : op_end; }
        uint8_t get_u8() { return src < src_end ? *src++ : 0; }

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
            if (scope.op == op_loop)
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
                        auto n = ip[0].u32;
                        auto return_ip = sp[N + 1 - 2 - n].ip;
                        auto return_sp = sp[N + 1 - 1 - n].sp;
                        for (uint32_t i = 0; i < n; ++i)
                            sp[i] = sp[N + 1 - n + i];

                        return_ip->code(return_ip + 1, return_sp);
                    }, uint32_t(self->return_count));
            };

            while (n == N) switch (get_code())
            {
            case op_unreachable:
                emit(trap);
                continue;

            case op_nop:
                continue;

            case op_block:
                *--bp = { op_block, get_u8(), dst, dst_begin };
                continue;
            case op_loop:
                *--bp = { op_loop, get_u8(), dst, dst_begin };
                continue;
            case op_if:
                *--bp = { op_if, get_u8(), dst, dst_begin };
                emit(do_forward_if, dst - dst);
                return N - 1;

            case op_else:
                // fix if branch
                (uint32_t&)bp[0].enter[1] = uint32_t(dst - bp[0].enter);
                // branch to leave
                do_branch(bp[0]);
                continue;

            case op_end:
                // fix up forward branches
                for (auto np = bp[0].leave; np != dst_begin;)
                {
                    auto offset = np->u32;
                    np->u32 = uint32_t(dst - np) + 1;

                    np = dst_begin + offset;
                }
                
                if (bp == &blocks[15])
                {
                    emit_return();
                    return 0;
                }
                ++bp;
                continue;

            case op_br:
                do_branch(bp[get_leb128_u32()]);
                continue;

            case op_br_if:
            {
                auto& scope = bp[get_leb128_u32()];
                if (scope.op == op_loop)
                {
                    // branch back
                    emit([](ip_t ip, sp_t sp) { auto offset = sp[N].i32 == 0 ? 0 : operand_u32(ip); (ip - offset + 1)->code(ip - offset + 2, sp); }, dst - scope.enter + 2);
                }
                else
                {
                    emit(do_forward_if, scope.leave - dst_begin);
                    scope.leave = dst - 1;
                }
                return N - 1;
            }

            case op_return:
                emit_return();
                continue;
            case op_call:
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
            case op_drop: return N - 1;
            case op_select:
                emit([](ip_t ip, sp_t sp) { if (sp[N].i32 != 0) sp[N - 2] = sp[N - 1]; ip->code(ip + 1, sp); });
                return N - 2;
            case op_local_get:
                emit([](ip_t ip, sp_t sp) { sp[N + 1] = sp[operand_u32(ip)]; ip[1].code(ip + 2, sp); }, get_leb128_u32());
                return compile<N + 1>(N + 1);
            case op_local_set:
                emit([](ip_t ip, sp_t sp) { sp[operand_u32(ip)] = sp[N]; ip[1].code(ip + 2, sp); }, get_leb128_u32());
                return N - 1;
            case op_local_tee:
                emit([](ip_t ip, sp_t sp) { sp[operand_u32(ip)] = sp[N]; ip[1].code(ip + 2, sp); }, get_leb128_u32());
                continue;

            case op_i32_const:
                emit([](ip_t ip, sp_t sp) { sp[N + 1].i32 = ip->i32; ip[1].code(ip + 2, sp); }, get_leb128_i32());
                return compile<N + 1>(N + 1);
            case op_i64_const:
                emit([](ip_t ip, sp_t sp) { sp[N + 1].i64 = ip->i64; ip[1].code(ip + 2, sp); }, get_leb128_u64());
                return compile<N + 1>(N + 1);


            // 32 bit booleans
            case op_i32_eqz: emit([](ip_t ip, sp_t sp) { sp[N].u32 = sp[N].u32 == 0 ? 1 : 0; ip->code(ip + 1, sp); }); continue;

            case op_i32_eq: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 == sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_ne: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 != sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_lt_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 < sp[N].i32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_lt_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 < sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_gt_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 > sp[N].i32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_gt_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 > sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_le_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 <= sp[N].i32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_le_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 <= sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_ge_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 >= sp[N].i32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_ge_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 >= sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;

            // 64 bit booleans
            case op_i64_eqz: emit([](ip_t ip, sp_t sp) { sp[N].u64 = sp[N].u64 == 0 ? 1 : 0; ip->code(ip + 1, sp); }); continue;

            case op_i64_eq: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 == sp[N].u64 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i64_ne: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 != sp[N].u64 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i64_lt_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i64 = sp[N - 1].i64 < sp[N].i64 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i64_lt_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 < sp[N].u64 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i64_gt_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i64 = sp[N - 1].i64 > sp[N].i64 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i64_gt_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 > sp[N].u64 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i64_le_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i64 = sp[N - 1].i64 <= sp[N].i64 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i64_le_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 <= sp[N].u64 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i64_ge_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i64 = sp[N - 1].i64 >= sp[N].i64 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;
            case op_i64_ge_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 >= sp[N].u32 ? 1 : 0; ip->code(ip + 1, sp); }), N - 1;

            // 32 bit alu

            case op_i32_clz: emit([](ip_t ip, sp_t sp) { sp[N].u32 = __lzcnt(sp[N].u32); ip->code(ip + 1, sp); }); continue;
            case op_i32_ctz: emit([](ip_t ip, sp_t sp) { sp[N].u32 = _tzcnt_u32(sp[N].u32); ip->code(ip + 1, sp); }); continue;
            case op_i32_popcnt: emit([](ip_t ip, sp_t sp) { sp[N].u32 = __popcnt(sp[N].u32); ip->code(ip + 1, sp); }); continue;                

            case op_i32_add:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 + sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_sub:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 - sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_mul:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 * sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_div_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 / sp[N].i32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_div_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 / sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_rem_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 % sp[N].i32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_rem_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 % sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_and:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 & sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_or:    return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 | sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_xor:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 ^ sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_shl:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 << sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_shr_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i32 = sp[N - 1].i32 >> sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_shr_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = sp[N - 1].u32 >> sp[N].u32; ip->code(ip + 1, sp); }), N - 1;
            case op_i32_rotl:  return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = _rotl(sp[N - 1].u32, sp[N].i32); ip->code(ip + 1, sp); }), N - 1;
            case op_i32_rotr:  return emit([](ip_t ip, sp_t sp) { sp[N - 1].u32 = _rotr(sp[N - 1].u32, sp[N].i32); ip->code(ip + 1, sp); }), N - 1;


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
            *bp = { op_block, bt_i32, dst, dst_begin };

            function fs[8];
            functions = fs;

            functions[0] = { dst, n, 1 };
            self = &functions[0];

            compile<2>(n + 1);
            return dst - dst_begin;
        }
    };
}
