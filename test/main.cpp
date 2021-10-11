#include "../inc/cjwasm/compiler.h"


uint8_t src[]
{ 
    cjwasm::wasm::op_local_get, 0, 
    cjwasm::wasm::op_local_get, 1, 
    cjwasm::wasm::op_mul_i32, 
    cjwasm::wasm::op_local_get, 0, 
    cjwasm::wasm::op_add_i32,
    cjwasm::wasm::op_end
};

cjwasm::code_t dst[1024];

int main()
{
    cjwasm::compiler c;

    c.compile_function(2, sizeof(src), src, 1024, dst);

    auto mul_add = [](int x, int y)
    {
        return cjwasm::call<int>(dst, x, y);
    };

    return mul_add(10, 4);
}