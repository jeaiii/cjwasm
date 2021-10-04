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

cjwasm::value_t stack[1024];

int main()
{
    cjwasm::compiler c;

    c.compile_function(2, sizeof(src), src, 1024, dst);

    auto wasm_mul_add = (cjwasm::code_t const*)dst;

    stack[0].i32 = 10;
    stack[1].i32 = 4;
    stack[2].ip = nullptr;
    stack[3].sp = nullptr;

    cjwasm::run(wasm_mul_add, stack);


}