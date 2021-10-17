#include "../inc/cjwasm/compiler.h"

uint8_t src1[]
{ 
    cjwasm::wasm::op_local_get, 0, 
    cjwasm::wasm::op_local_get, 1, 
    cjwasm::wasm::op_mul_i32, 
    cjwasm::wasm::op_local_get, 0, 
    cjwasm::wasm::op_add_i32,
    cjwasm::wasm::op_end
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

uint8_t src2[]
{
    cjwasm::wasm::op_block, cjwasm::wasm::bt_void,
    cjwasm::wasm::op_local_get, 1,
    cjwasm::wasm::op_const_i32, 2,
    cjwasm::wasm::op_lt_s_i32,
    cjwasm::wasm::op_br_if, 0,

    cjwasm::wasm::op_loop, cjwasm::wasm::bt_void,
    cjwasm::wasm::op_local_get, 1,
    cjwasm::wasm::op_local_get, 0,
    cjwasm::wasm::op_mul_i32,
    cjwasm::wasm::op_local_set, 0,
    cjwasm::wasm::op_local_get, 1,
    cjwasm::wasm::op_const_i32, uint8_t(-1),
    cjwasm::wasm::op_add_i32,
    cjwasm::wasm::op_local_tee, 1,
    cjwasm::wasm::op_const_i32, 1,
    cjwasm::wasm::op_gt_s_i32, 0,
    cjwasm::wasm::op_br_if, 0,
    cjwasm::wasm::op_end,
    cjwasm::wasm::op_end,
    cjwasm::wasm::op_local_get, 0,
    cjwasm::wasm::op_end,
};

cjwasm::code_t g_dst[1024];

template<int N> void compile(uint8_t(&src)[N], cjwasm::code_t dst[])
{
    cjwasm::compiler c;
    c.compile_function(2, N, src, 1024, dst);
}

int main()
{
    compile(src2, g_dst);

    auto mul_add = [](int x, int y)
    {
        return cjwasm::call<int>(g_dst, x, y);
    };

    return mul_add(10, 5);
}