#include "../inc/cjwasm/compiler.h"

cjwasm::uint8_t src1[]
{ 
    cjwasm::op_local_get, 0, 
    cjwasm::op_local_get, 1, 
    cjwasm::op_i32_mul, 
    cjwasm::op_local_get, 0, 
    cjwasm::op_i32_add,
    cjwasm::op_end
};

/*
int f(int x, int y)
{
    while (y > 1)
        x *= y, --y;
    return x;
}

block $B0
get_local $p0
i32.const 2
i32.lt_s
br_if $B0
loop $L1
get_local $p1
get_local $p0
i32.mul
set_local $p1
get_local $p0
i32.const - 1
i32.add
tee_local $p0
i32.const 1
i32.gt_s
br_if $L1
end
end
get_local $p1)
*/

cjwasm::uint8_t src2[]
{
    cjwasm::op_block, cjwasm::bt_void,
    cjwasm::op_local_get, 0,
    cjwasm::op_i32_const, 2,
    cjwasm::op_i32_lt_s,
    cjwasm::op_br_if, 0,

    cjwasm::op_loop, cjwasm::bt_void,
    cjwasm::op_local_get, 1,
    cjwasm::op_local_get, 0,
    cjwasm::op_i32_mul,
    cjwasm::op_local_set, 1,
    cjwasm::op_local_get, 0,
    cjwasm::op_i32_const, 0x7f, // -1 in leb128s 
    cjwasm::op_i32_add,
    cjwasm::op_local_tee, 0,
    cjwasm::op_i32_const, 1,
    cjwasm::op_i32_gt_s,
    cjwasm::op_br_if, 0,
    cjwasm::op_end,
    cjwasm::op_end,
    cjwasm::op_local_get, 1,
};

cjwasm::uint8_t src3[] =
{
    0, 97, 115, 109, 1, 0, 0, 0,
    1, 136, 128, 128, 128, 0, 1, 96, 3, 127, 127, 127, 1, 127, 3, 130, 128, 128, 128, 0, 1, 0, 4, 132, 128, 128, 128, 0, 1, 112, 0, 0, 5, 131, 128, 128, 128, 0, 1, 0, 1, 6, 129, 128, 128, 128, 0, 0, 7, 143, 128, 128, 128, 0, 2, 6, 109, 101, 109, 111, 114, 121, 2, 0, 2, 109, 97, 0, 0, 10, 144, 128, 128, 128, 0, 1, 138, 128, 128, 128, 0, 0, 32, 1, 32, 0, 108, 32, 2, 106, 11
};

cjwasm::uint8_t src_fib[] =
{
    0, 97, 115, 109, 1, 0, 0, 0,
    1, 134, 128, 128, 128, 0, 1, 96, 1, 127, 1, 127, 3, 130, 128, 128, 128, 0, 1, 0, 4, 132, 128, 128, 128, 0, 1, 112, 0, 0, 5, 131, 128, 128, 128, 0, 1, 0, 1, 6, 129, 128, 128, 128, 0, 0, 7, 144, 128, 128, 128, 0, 2, 6, 109, 101, 109, 111, 114, 121, 2, 0, 3, 102, 105, 98, 0, 0, 10, 164, 128, 128, 128, 0, 1, 158, 128, 128, 128, 0, 0, 2, 64, 32, 0, 65, 2, 78, 13, 0, 32, 0, 15, 11, 32, 0, 65, 127, 106, 16, 0, 32, 0, 65, 126, 106, 16, 0, 106, 11
};

cjwasm::code_t g_dst[1024];

template<unsigned N> void compile(cjwasm::uint8_t (&src)[N], cjwasm::code_t dst[])
{
    cjwasm::compiler c;
    c.compile_function(2, N, src, 1024, dst);
}

#include <iostream>


bool parse(unsigned size, uint8_t const data[])
{
    cjwasm::source s3{ data, data, data + size };

    int f0_argc = 0;
    int f0_retc = 0;

    auto out_type = [](uint8_t bt)
    {
        switch (bt)
        {
        case cjwasm::bt_i32: std::cout << "i32"; break;
        case cjwasm::bt_i64: std::cout << "i64"; break;
        case cjwasm::bt_f32: std::cout << "f32"; break;
        case cjwasm::bt_f64: std::cout << "f64"; break;
        default: std::cout << "???"; break;
        }
    };

    auto underflow = [&]()
    {
        return s3.src >= s3.src_end;
    };

    if (s3.get_u8() == '\0' && s3.get_u8() == 'a' && s3.get_u8() == 's' && s3.get_u8() == 'm')
    {
        uint32_t version = s3.get_u8() + (uint32_t(s3.get_u8()) << 8) + (uint32_t(s3.get_u8()) << 16) + (uint32_t(s3.get_u8()) << 24);
        std::cout << "wasm file version " << version << std::endl;
        if (version != 1)
        {
            std::cout << "ERROR - bad version\n";
            return false;
        }
    }
    else
    {
        std::cout << "ERROR - bad wasm file\n";
        return false;
    }
    for (;s3.src < s3.src_end;)
    {
        int section = s3.get_u8();

        uint32_t length = s3.get_leb128_u32();
        if (s3.src + length > s3.src_end)
        {
            std::cout << "ERROR - section length out of scope\n";
            return false;
        }
        switch (section)
        {
        case 0:
            std::cout << section <<": custom section [" << length << "]\n";
            s3.src += length;
            break;
        case 1:
            std::cout << section << ": type section [" << length << "]\n";
            for (uint32_t count = s3.get_leb128_u32(), i = 0; i < count; ++i)
            {
                if (s3.get_u8() != 0x60)
                {
                    std::cout << "ERROR - expected function type != 0x60\n";
                    return false;

                }

                uint32_t argc = s3.get_leb128_u32();
                if (underflow()) 
                    return false;

                f0_argc = argc;

                std::cout << "    type #" << i << ": func (";
                for (uint32_t i = 0; i < argc; ++i)
                {
                    if (i != 0)
                        std::cout << ", ";
                    out_type(s3.get_u8());
                    if (underflow())
                        return false;
                }
                std::cout << ") => (";
                uint32_t retc = s3.get_leb128_u32();
                if (underflow())
                    return false;

                f0_retc = retc;
                for (uint32_t i = 0; i < retc; ++i)
                {
                    if (i != 0)
                        std::cout << ", ";
                    out_type(s3.get_u8());
                    if (underflow())
                        return false;
                }
                std::cout << ")\n";

                if (underflow())
                    return false;
            }
            break;
        case 2:
            std::cout << section << ": import section [" << length << "]\n";
            s3.src += length;
            break;
        case 3:
            std::cout << section << ": function section [" << length << "]\n";
            for (uint32_t count = s3.get_leb128_u32(), i = 0; i < count; ++i)
            {
                std::cout << "    function #" << i << ": typeidx = " << s3.get_leb128_u32() << "\n";
            }
            break;
        case 4:
            std::cout << section << ": table section [" << length << "]\n";
            s3.src += length;
            break;
        case 5:
            std::cout << section << ": memory section [" << length << "]\n";
            s3.src += length;
            break;
        case 6:
            std::cout << section << ": global section [" << length << "]\n";
            s3.src += length;
            break;
        case 7:
            std::cout << section << ": export section [" << length << "]\n";
            for (uint32_t count = s3.get_leb128_u32(), i = 0; i < count; ++i)
            {
                std::cout << "    export #" << i << ": name = '";
                uint32_t name_length = s3.get_leb128_u32();
                for (uint32_t i = 0; i < name_length; ++i)
                    std::cout << s3.get_u8();
                std::cout << "', ";
                int kind = s3.get_u8();
                uint32_t idx = s3.get_leb128_u32();
                switch (kind)
                {
                case 0: std::cout << "funcidx_0 = "; break;
                case 1: std::cout << "tableidx_1 = "; break;
                case 2: std::cout << "memidx_2 = "; break;
                case 3: std::cout << "globalidx_3 = "; break;
                default: std::cout << "???inx_" << kind << " = "; break;
                }
                std::cout << idx << "\n";
            }
            break;
        case 8:
            std::cout << section << ": start section [" << length << "]\n";
            s3.src += length;
            break;
        case 9:
            std::cout << section << ": element section [" << length << "]\n";
            s3.src += length;
            break;
        case 10:
            std::cout << section << ": code section [" << length << "]\n";
            for (uint32_t count = s3.get_leb128_u32(), i = 0; i < count; ++i)
            {
                uint32_t size = s3.get_leb128_u32();
                auto end = s3.src + size;
                uint32_t localc = s3.get_leb128_u32();
                std::cout << "    code #" << i << ": locals = " << localc;
                for (uint32_t i = 0; i < localc; ++i)
                {
                    uint32_t n = s3.get_leb128_u32();
                    uint8_t type = s3.get_u8();
                    std::cout << ", [" << n << "; ";
                    out_type(type);
                    std::cout << "]";
                }
                std::cout << "\n";

                cjwasm::compiler c;
                c.compile_function(f0_argc, end - s3.src, s3.src, 1024, g_dst);

                s3.src = end;
            }
            break;
        case 11:
            std::cout << section << ": data section [" << length << "]\n";
            s3.src += length;
            break;
        case 12:
            std::cout << section << ": data count section [" << length << "]\n";
            s3.src += length;
            break;
        default:
            std::cout << section << ": unknown section [" << length << "]\n";
            s3.src += length;
            break;
        }
    }

    return true;
}

template<unsigned N> void parse(uint8_t const (&src)[N]) { parse(N, src); }

int main()
{
    compile(src2, g_dst);
    cjwasm::fn<int, int, int> test{ g_dst };
    auto t = test(5, 5);
    std::cout << "test = " << t << "\n";

    parse(src3);
    cjwasm::fn<int, int, int, int> ma{ g_dst };
    auto m = ma(2, 4, 5);
    std::cout << "ma(3, 4, 5) -> " << m << "\n";

    parse(src_fib);
    cjwasm::fn<int, int> fib{ g_dst };
    auto f = fib(7);
    std::cout << "fib = " << f << "\n";

    return 0;
}