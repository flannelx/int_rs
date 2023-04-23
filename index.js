import init, { run } from './wasm/pkg/wasm.js';
await init();
const input_ta = document.getElementById('input');
input_ta.value =
`let x = 1;
let z = x + 2;
let hello = "Hello ";
fn add(x, y) {
    x + y;
};
add(hello, "World! z = " + z);`

const output = document.getElementById('output');
document.getElementById('run').onclick = async () => {
    let ret = await run(input_ta.value);
    output.innerHTML = ret;
    console.log(ret)
};


document.getElementById('reset').onclick = async () => {
    output.innerHTML = "";
    input_ta.value = "";
}

