namespace cjwasm
{
    using uint8_t = unsigned char;
    using uint16_t = unsigned short;
    using uint32_t = decltype(0xffffffff);
    using uint64_t = decltype(0xffffffffffffffff);
    using int8_t = signed char;
    using int16_t = short;
    using int32_t = decltype(2147483647);
    using int64_t = decltype(9223372036854775807);

    extern "C" uint32_t _lzcnt_u32(uint32_t);
    extern "C" uint32_t _tzcnt_u32(uint32_t);
    extern "C" uint32_t __popcnt(uint32_t);
    extern "C" uint32_t _rotl(uint32_t, int);
    extern "C" uint32_t _rotr(uint32_t, int);

    extern "C" uint64_t _lzcnt_u64(uint64_t);
    extern "C" uint64_t _tzcnt_u64(uint64_t);
    extern "C" uint64_t __popcnt64(uint64_t);
    extern "C" uint64_t _rotl64(uint64_t, int);
    extern "C" uint64_t _rotr64(uint64_t, int);

    enum : uint8_t
    {
        op_unreachable, op_nop, op_block, op_loop, op_if, op_else, _op_06, _op_07, _op_08, _op_09, _op_0a, op_end, op_br, op_br_if, _op_br_table, op_return,
        
        op_call, _op_call_indirect, _op_12, _op_13, _op_14, _op_15, _op_16, _op_17, _op_18, _op_19, op_drop, op_select, _op_select_t, _op_1d, _op_1e, _op_1f,

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

        op_i64_clz, op_i64_ctz, op_i64_popcnt,
        op_i64_add, op_i64_sub, op_i64_mul, op_i64_div_s, op_i64_div_u, op_i64_rem_s, op_i64_rem_u,
        op_i64_and, op_i64_or, op_i64_xor, op_i64_shl, op_i64_shr_s, op_i64_shr_u, op_i64_rotl, op_i64_rotr,

        _op_f32_abs, _op_f32_neg, _op_f32_ceil, _op_f32_floor, _op_f32_trunc, _op_f32_nearest, _op_f32_sqrt, _op_f32_add, _op_f32_sub, _op_f32_mul, _op_f32_div, _op_f32_min, _op_f32_max, _op_f32_copysign,
        _op_f64_abs, _op_f64_neg, _op_f64_ceil, _op_f64_floor, _op_f64_trunc, _op_f64_nearest, _op_f64_sqrt, _op_f64_add, _op_f64_sub, _op_f64_mul, _op_f64_div, _op_f64_min, _op_f64_max, _op_f64_copysign,

        op_i32_wrap_i64,
        _op_i32_trunc_f32_s, _op_i32_trunc_f32_u, _op_i32_trunc_f64_s, _op_i32_trunc_f64_u,
        op_i64_extend_i32_s, op_i64_extend_i32_u, 
        
        _op_i64_trunc_f32_s, _op_i64_trunc_f32_u, _op_i64_trunc_f64_s, _op_i64_trunc_f64_u,
        _op_f32_convert_i32_s, _op_f32_convert_i32_u, _op_f32_convert_i64_s, _op_f32_convert_i64_u, _op_f32_demote_f64,
        _op_f64_convert_i32_s, _op_f64_convert_i32_u, _op_f64_convert_i64_s, _op_f64_convert_i64_u, _op_f64_promote_f32,
        _op_i32_reinterpret_f32, _op_i64_reinterpret_f64, _op_f32_reinterpret_i32, _op_f64_reinterpret_i64,

        op_i32_extend8_s, op_i32_extend16_s, 
        op_i64_extend8_s, op_i64_extend16_s, op_i64_extend32_s,

        _op_c5, _op_c6, _op_c7, _op_c8, _op_c9, _op_ca, _op_cb, _op_cc, _op_cd, _op_ce, _op_cf,

        _op_ref_null, _op_ref_is_null, _op_ref_func,

        _op_fc = 0xfc,

        _fc_i32_trunc_sat_f32_s = 0, _fc_i32_trunc_sat_f32_u, _fc_i32_trunc_sat_f64_s, _fc_i32_trunc_sat_f64_u, _fc_i64_trunc_sat_f32_s, _fc_i64_trunc_sat_f32_u, _fc_i64_trunc_sat_f64_s, _fc_i64_trunc_sat_f64_u,
        _fc_memory_init,
        _fc_data_drop,
        _fc_memory_copy,
        _fc_memory_fill,
        _fc_table_init,
        _fc_elem_drop,
        _fc_table_copy,
        _fc_table_grow,
        _fc_table_size,
        _fc_table_fill,

        _op_fd = 0xfd, //simd

        bt_void = 0x40,
        bt_i32 = 0x7f, bt_i64 = 0x7e, bt_f32 = 0x7d, bt_f64 = 0x7c,
    };

    union value_t
    {
        uint64_t u64;
        int64_t i64;
        uint32_t u32;
        int32_t i32;
        void (*code)(value_t const*, value_t*);
        value_t const* ip;
        value_t* sp;
        value_t* vp;
    };

    using sp_t = value_t*;
    using ip_t = value_t const*;

    using code_t = value_t;

    struct runtime_t
    {
        value_t values[512];
        char memory[64 * 1024];

        struct m2 { char data[2]; };
        struct m4 { char data[4]; };
        struct m8 { char data[8]; };

        uint64_t load8u(unsigned n) { return *reinterpret_cast<uint8_t const*>(memory + n); }
        uint64_t load16u(unsigned n) { return __builtin_bit_cast(uint16_t, *reinterpret_cast<m2 const*>(memory + n)); }
        uint64_t load32u(unsigned n) { return __builtin_bit_cast(uint32_t, *reinterpret_cast<m4 const*>(memory + n)); }
        uint64_t load8s(unsigned n) { return *reinterpret_cast<int8_t const*>(memory + n); }
        uint64_t load16s(unsigned n) { return __builtin_bit_cast(int16_t, *reinterpret_cast<m2 const*>(memory + n)); }
        uint64_t load32s(unsigned n) { return __builtin_bit_cast(int32_t, *reinterpret_cast<m4 const*>(memory + n)); }
        uint64_t load64(unsigned n) { return __builtin_bit_cast(uint64_t, *reinterpret_cast<m8 const*>(memory + n)); }

        void store8(unsigned n, uint64_t x) { *reinterpret_cast<uint8_t*>(memory + n) = uint8_t(x); }
        void store16(unsigned n, uint64_t x) { *reinterpret_cast<m2*>(memory + n) = __builtin_bit_cast(m2, uint16_t(x)); }
        void store32(unsigned n, uint64_t x) { *reinterpret_cast<m4*>(memory + n) = __builtin_bit_cast(m4, uint32_t(x)); }
        void store64(unsigned n, uint64_t x) { *reinterpret_cast<m8*>(memory + n) = __builtin_bit_cast(m8, x); }
    };

    using rp_t = runtime_t*;

    #define CJWASM_ARGS ip_t ip, sp_t sp
    #define CJWASM_NEXT() ip->code(ip + 1, sp)

    inline void trap(CJWASM_ARGS) { }

    struct function
    {
        ip_t ip;
        int argument_count;
        int return_count;

        static constexpr struct
        {
            value_t call;
            value_t delta;
            void (&code)(CJWASM_ARGS);
        } 
        common_return{ 0, 0, trap };
    };

    template<class> struct fn;
    template<class T, class...Ts> struct fn<T(Ts...)>
    {
        ip_t ip;

        T operator()(Ts...ts) const
        {
            union
            {
                struct { value_t values[sizeof...(Ts) + 1]; } frame;
                value_t stack[256];
            };

            frame = { uint64_t(ts)..., uint64_t(&function::common_return.call) };
            ip->code(ip + 1, stack);
            return (T const&)stack[0];
        }
    };

    struct source
    {
        uint8_t const* src_begin;
        uint8_t const* src;
        uint8_t const* src_end;

        uint8_t get_op() { return src < src_end ? *src++ : op_end; }
        uint8_t get_u8() { return src < src_end ? *src++ : 0; }

        // https://en.wikipedia.org/wiki/LEB128
        uint32_t get_leb128_u32()
        {
            uint32_t u0 = get_u8();
            if (u0 < 128)
                return u0;
            uint32_t u1 = get_u8();
            if (u1 < 128)
                return (u1 << 7) ^ u0 ^ 128;
            uint32_t u2 = get_u8();
            if (u2 < 128)
                return (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x00004080;
            uint32_t u3 = get_u8();
            if (u3 < 128)
                return (u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x00204080;
            uint32_t u4 = get_u8();
            return (u4 << 28) ^ (u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x10204080;
        }

        // returns uint32 on purpose
        uint32_t get_leb128_i32()
        {
            uint32_t u0 = get_u8();
            if (u0 < 128)
                return int32_t(u0 << (32 - 7)) >> (32 - 7);
            uint32_t u1 = get_u8();
            if (u1 < 128)
                return int32_t(((u1 << 7) ^ u0 ^ 128) << (32 - 14)) >> (32 - 14);
            uint32_t u2 = get_u8();
            if (u2 < 128)
                return int32_t(((u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x00004080) << (32 - 21)) >> (32 - 21);
            uint32_t u3 = get_u8();
            if (u3 < 128)
                return int32_t(((u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x00204080) << (32 - 28)) >> (32 - 28);
            uint32_t u4 = get_u8();
            return int32_t((u4 << 28) ^ (u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x10204080);
        }

        // returns uint64 on purpose
        uint64_t get_leb128_i64()
        {
            uint64_t u0 = get_u8();
            if (u0 < 128)
                return int64_t(u0 << (64 - 7)) >> (64 - 7);
            uint64_t u1 = get_u8();
            if (u1 < 128)
                return int64_t(((u1 << 7) ^ u0 ^ 128) << (64 - 14)) >> (64 - 14);
            uint64_t u2 = get_u8();
            if (u2 < 128)
                return int64_t(((u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x00004080) << (64 - 21)) >> (64 - 21);
            uint64_t u3 = get_u8();
            if (u3 < 128)
                return int64_t(((u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x00204080) << (64 - 28)) >> (64 - 28);
            uint64_t u4 = get_u8();
            if (u4 < 128)
                return int64_t(((u4 << 28) ^ (u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x10204080) << (64 - 35)) >> (64 - 35);
            uint64_t u5 = get_u8();
            if (u5 < 128)
                return int64_t(((u5 << 35) ^ (u4 << 28) ^ (u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x810204080) << (64 - 42)) >> (64 - 42);
            uint64_t u6 = get_u8();
            if (u6 < 128)
                return int64_t(((u6 << 42) ^ (u5 << 35) ^ (u4 << 28) ^ (u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x40810204080) << (64 - 49)) >> (64 - 49);
            uint64_t u7 = get_u8();
            if (u7 < 128)
                return int64_t(((u7 << 49) ^ (u6 << 42) ^ (u5 << 35) ^ (u4 << 28) ^ (u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x2040810204080) << (64 - 56)) >> (64 - 56);
            uint64_t u8 = get_u8();
            if (u8 < 128)
                return int64_t(((u8 << 56) ^ (u7 << 49) ^ (u6 << 42) ^ (u5 << 35) ^ (u4 << 28) ^ (u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x102040810204080) << (64 - 63)) >> (64 - 63);
            uint64_t u9 = get_u8();
            return int64_t((u9 << 63) ^ (u8 << 56) ^ (u7 << 49) ^ (u6 << 42) ^ (u5 << 35) ^ (u4 << 28) ^ (u3 << 21) ^ (u2 << 14) ^ (u1 << 7) ^ u0 ^ 0x8102040810204080);
        }
    };

    struct compiler : source
    {
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

        block blocks[256];
        block* bp;

        static uint32_t operand_u32(ip_t ip) { return ip->u32; }

        void emit(uint32_t a) { dst[0].u32 = a; ++dst; }
        void emit(uint64_t a) { dst[0].u64 = a; ++dst; }
        void emit(void (*f)(CJWASM_ARGS)) { dst[0].code = f; ++dst; }
        void emit(void (*f)(CJWASM_ARGS), uint32_t a) { emit(f), emit(a); }
        void emit(void (*f)(CJWASM_ARGS), uint64_t a) { emit(f), emit(a); }
        void emit(void (*f)(CJWASM_ARGS), decltype("" - "") a) { emit(f), emit(uint32_t(a)); }
        void emit(ip_t ip) { dst[0].ip = ip; ++dst; }

        // unconditional branches don't depend on stack depth so handle them outside compile so there aren't unique ones per stack depth
        void emit_branch(block& scope)
        {
            if (scope.op == op_loop)
            {
                // branch back
                emit([](CJWASM_ARGS) { auto offset = operand_u32(ip); (ip - offset + 1)->code(ip - offset + 2, sp); }, dst - scope.enter + 2);
            }
            else
            {
                // branch forward
                emit([](CJWASM_ARGS) { auto offset = operand_u32(ip); (ip + offset + 1)->code(ip + offset + 2, sp); }, dst - scope.leave);
                scope.leave = dst - 1;
            }
        }

        template<int N, int Ac> void compile_call(function const& f)
        {
            if constexpr (N > Ac)
            {
                if (f.argument_count > Ac)
                    compile_call<N, Ac + 1>(f);
                else
                    emit([](CJWASM_ARGS)
                        {
                            sp[N + 1].ip = ip;
                            
                            ip = ip[0].ip;
                            sp += 1 + N - Ac;

                            CJWASM_NEXT();                            
                        }), emit(f.ip), emit(uint32_t(1 + N - Ac));
            }
        }

        template<int N, int Rc> void compile_return()
        {
            if constexpr (N > Rc)
            {
                if (self->return_count > Rc)
                    compile_return<N, Rc + 1>();
                else
                    emit([](CJWASM_ARGS)
                        {
                            ip = sp[N - Rc].ip + 2;

                            for (int i = 0; i < Rc; ++i)
                                sp[i] = sp[1 + N - Rc + i];

                            sp -= ip[-1].u32;

                            CJWASM_NEXT();
                        });
            }
        }

        template<int N>
        int compile(int n)
        {
            auto emit_forward_if = [this](decltype("" - "") a)
            {
                emit([](CJWASM_ARGS)
                {
                    auto offset = operand_u32(ip);
                    auto jump = sp[N].i32 != 0 ? offset : 0;

                    ip += jump;
                    ip += 2;
                    ip[-1].code(ip, sp);

                    }, uint32_t(a));
            };

            for (;;)
            {

                if (n < N)
                    return n;

                if (n > N)
                    n = compile<N + 1>(n);

                while (n == N)
                {
                    auto op = get_op();
                    switch (op)
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
                        emit_forward_if(0);
                        return N - 1;

                    case op_else:
                        // fix if branch
                        (uint32_t&)bp[0].enter[1] = uint32_t(dst - bp[0].enter);
                        // branch to leave
                        emit_branch(bp[0]);
                        continue;

                    case op_end:
                        // fix up forward branches
                        // BUG
#if 0
                        for (auto np = bp[0].leave; np != dst_begin;)
                        {
                            auto offset = np->u32;
                            np->u32 = uint32_t(dst - np) - 1;// +1;

                            np = dst_begin + offset;
                        }
#endif

                        if (bp == &blocks[255])
                        {
                            compile_return<N, 0>();
                            return 0;
                        }
                        ++bp;
                        continue;

                    case op_br:
                        emit_branch(bp[get_leb128_u32()]);
                        continue;

                    case op_br_if:
                    {
                        auto& scope = bp[get_leb128_u32()];
                        if (scope.op == op_loop)
                        {
                            // branch back
                            emit([](CJWASM_ARGS)
                                { 
                                    auto offset = sp[N].i32 == 0 ? 0 : operand_u32(ip); 
                                    ++ip;
                                    ip -= offset;
                                    CJWASM_NEXT();
                                }, dst - scope.enter + 2);
                        }
                        else
                        {
                            emit_forward_if(scope.leave - dst_begin);
                            scope.leave = dst - 1;
                        }
                        return N - 1;
                    }
                    case _op_br_table:
                    {
                        auto n = get_leb128_u32();
                        for (uint32_t i = 0; i < n; ++i)
                            (void)get_leb128_u32();
                        (void)get_leb128_u32();
                        continue;
                    }
                    case op_return:
                        compile_return<N, 0>();
                        n -= self->return_count;
                        continue;
                    case op_call:
                    {
                        auto fn = get_leb128_u32();
                        fn = 0; // TEST
                        auto const& f = functions[fn];
                        compile_call<N, 0>(f);
                        n += f.return_count - f.argument_count;
                        //n += 1;
                        continue;
                    }
                    case _op_call_indirect:
                    {
                        auto tableidx = get_leb128_u32();
                        auto typeidx = get_leb128_u32();
                        continue;
                    }

                    case op_drop: return N - 1;
                    case op_select:
                        emit([](CJWASM_ARGS) { if (sp[N].i32 != 0) sp[N - 2] = sp[N - 1]; CJWASM_NEXT(); });
                        return N - 2;
                    case op_local_get:
                    {
                        auto slot = get_leb128_u32();
                        switch (slot)
                        {
                        case 0: emit([](CJWASM_ARGS) { sp[N + 1] = sp[0]; CJWASM_NEXT(); }); break;
                        case 1: emit([](CJWASM_ARGS) { sp[N + 1] = sp[1]; CJWASM_NEXT(); }); break;
                        case 2: emit([](CJWASM_ARGS) { sp[N + 1] = sp[2]; CJWASM_NEXT(); }); break;
                        case 3: emit([](CJWASM_ARGS) { sp[N + 1] = sp[3]; CJWASM_NEXT(); }); break;
                        default: emit([](CJWASM_ARGS) { sp[N + 1] = sp[operand_u32(ip)]; ++ip;  CJWASM_NEXT(); }, slot); break;
                        }
                        ++n;
                        continue;

                    }
                    case op_local_set:
                        emit([](CJWASM_ARGS) { sp[operand_u32(ip)] = sp[N]; ++ip; CJWASM_NEXT(); }, get_leb128_u32());
                        return N - 1;
                    case op_local_tee:
                        emit([](CJWASM_ARGS) { sp[operand_u32(ip)] = sp[N]; ++ip; CJWASM_NEXT(); }, get_leb128_u32());
                        continue;

                    // TODO add env
                    case _op_global_set:
                        emit([](CJWASM_ARGS) { auto gp = sp[0].vp; gp[operand_u32(ip)] = sp[N];  ip[1].code(ip + 2, sp);  }, get_leb128_u32());
                        return N - 1;
                    case _op_global_get:
                        emit([](CJWASM_ARGS) { auto gp = sp[0].vp; sp[N + 1] = gp[operand_u32(ip)];  ip[1].code(ip + 2, sp);  }, get_leb128_u32());
                        ++n;
                        break;

                    case _op_i32_load:
                    case _op_i64_load:
                    case _op_f32_load:
                    case _op_f64_load:
                    case _op_i32_load8_s:
                    case _op_i32_load8_u:
                    case _op_i32_load16_s:
                    case _op_i32_load16_u:
                    case _op_i64_load8_s:
                    case _op_i64_load8_u:
                    case _op_i64_load16_s:
                    case _op_i64_load16_u:
                    case _op_i64_load32_s:
                    case _op_i64_load32_u:
                    {
                        auto load = [](CJWASM_ARGS)
                        {
                            sp[N].u32 = *(uint32_t const*)((char const*)(sp[0].vp) + sp[N].u32 + operand_u32(ip));
                            ip[1].code(ip + 2, sp);
                        };
                        auto offset = get_leb128_u32();
                        (void)get_leb128_u32(); // align
                        emit(load, offset);
                        break;
                    }

                    case _op_i32_store:
                    case _op_i64_store:
                    case _op_f32_store:
                    case _op_f64_store:
                    case _op_i32_store8:
                    case _op_i32_store16:
                    case _op_i64_store8:
                    case _op_i64_store16:
                    case _op_i64_store32:
                    {
                        auto offset = get_leb128_u32();
                        (void)get_leb128_u32(); // align
                        auto store = [](CJWASM_ARGS)
                        {
                            auto offset = operand_u32(ip);
                            (void)offset;
                            //TODO mem[sp[N-1] + offset] = sp[N];
                            ip[1].code(ip + 2, sp);
                        };
                        emit(store, offset);
                        return N - 2;
                    }

                    case op_i32_const:
                        emit([](ip_t ip, sp_t sp) { sp[N + 1].i32 = ip->i32; ip[1].code(ip + 2, sp); }, get_leb128_i32());
                        ++n;
                        continue;
                    case op_i64_const:
                        emit([](ip_t ip, sp_t sp) { sp[N + 1].i64 = ip->i64; ip[1].code(ip + 2, sp); }, get_leb128_i64());
                        ++n;
                        continue;

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
                    case op_i32_clz: emit([](ip_t ip, sp_t sp) { sp[N].u32 = _lzcnt_u32(sp[N].u32); ip->code(ip + 1, sp); }); continue;
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

                        // 64 bit alu
                    case op_i64_clz: emit([](ip_t ip, sp_t sp) { sp[N].u64 = _lzcnt_u64(sp[N].u64); ip->code(ip + 1, sp); }); continue;
                    case op_i64_ctz: emit([](ip_t ip, sp_t sp) { sp[N].u64 = _tzcnt_u64(sp[N].u64); ip->code(ip + 1, sp); }); continue;
                    case op_i64_popcnt: emit([](ip_t ip, sp_t sp) { sp[N].u64 = __popcnt64(sp[N].u64); ip->code(ip + 1, sp); }); continue;

                    case op_i64_add:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 + sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_sub:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 - sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_mul:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 * sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_div_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i64 = sp[N - 1].i64 / sp[N].i64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_div_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 / sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_rem_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i64 = sp[N - 1].i64 % sp[N].i64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_rem_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 % sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_and:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 & sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_or:    return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 | sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_xor:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 ^ sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_shl:   return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 << sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_shr_s: return emit([](ip_t ip, sp_t sp) { sp[N - 1].i64 = sp[N - 1].i64 >> sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_shr_u: return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = sp[N - 1].u64 >> sp[N].u64; ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_rotl:  return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = _rotl64(sp[N - 1].u64, sp[N].i32); ip->code(ip + 1, sp); }), N - 1;
                    case op_i64_rotr:  return emit([](ip_t ip, sp_t sp) { sp[N - 1].u64 = _rotr64(sp[N - 1].u64, sp[N].i32); ip->code(ip + 1, sp); }), N - 1;

                        // misc int
                    case op_i32_wrap_i64: /*emit([](ip_t ip, sp_t sp) { sp[N].i32 = sp[N].i64; ip->code(ip + 1, sp); });*/ continue;
                    case op_i64_extend_i32_s: emit([](ip_t ip, sp_t sp) { sp[N].i64 = sp[N].i32; ip->code(ip + 1, sp); }); continue;
                    case op_i64_extend_i32_u: emit([](ip_t ip, sp_t sp) { sp[N].u64 = sp[N].u32; ip->code(ip + 1, sp); }); continue;
                    case op_i32_extend8_s: emit([](ip_t ip, sp_t sp) { sp[N].i32 = int8_t(sp[N].u32); ip->code(ip + 1, sp); }); continue;
                    case op_i32_extend16_s: emit([](ip_t ip, sp_t sp) { sp[N].i32 = int16_t(sp[N].u32); ip->code(ip + 1, sp); }); continue;
                    case op_i64_extend8_s: emit([](ip_t ip, sp_t sp) { sp[N].i64 = int8_t(sp[N].u64); ip->code(ip + 1, sp); }); continue;
                    case op_i64_extend16_s: emit([](ip_t ip, sp_t sp) { sp[N].i64 = int16_t(sp[N].u64); ip->code(ip + 1, sp); }); continue;
                    case op_i64_extend32_s: emit([](ip_t ip, sp_t sp) { sp[N].i64 = int32_t(sp[N].u64); ip->code(ip + 1, sp); }); continue;

                    case _op_fc:
                    {
                        auto fc = get_op();
                        switch (fc)
                        {
                        case _fc_memory_copy:
                            continue;
                        case _fc_memory_fill:
                            continue;
                        default: __debugbreak();
                        }
                        continue;
                    }
                    default: __debugbreak();
                        continue;
                    }
                }
            }
        }
        // compile error
        template<> int compile<8>(int) { return 7; }

        size_t compile_function(int n, size_t src_size, uint8_t const src_data[], size_t dst_size, code_t dst_data[])
        {
            src_begin = src_data;
            src = src_begin;
            src_end = src + src_size;

            dst_begin = dst_data;
            dst = dst_begin;
            dst_end = dst + dst_size;

            bp = &blocks[255];
            *bp = { op_block, bt_i32, dst, dst_begin };

            function fs[8];
            functions = fs;

            functions[0] = { dst, n, 1 };
            self = &functions[0];

            compile<0>(n);
            return dst - dst_begin;
        }
    };
}
