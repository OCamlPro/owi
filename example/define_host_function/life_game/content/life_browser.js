console.log("Conway's Game of Life ;)")

let canvas = document.getElementById('lifeCanvas');
let ctx = canvas.getContext('2d');

let life_print_step = null;

let x = 0;
let y = 0;

function height (i) {
    return 240;
}

function width (i) {
    return 320;
}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function cell_print(i) {
    ctx.fillStyle = (i == 1) ? 'black' : 'white';
    ctx.fillRect(x*2, y*2, 2, 2);
    x = x + 1;
}

function clear_screen(i) {
    x = 0;
    y = 0;
}

function newline(i) {
    x = 0;
    y = y + 1;
}

function rand_val (bound) {
    let r = Math.floor(Math.random() * bound);
    return (r == 0) ? 1 : 0;
}

async function life() {
    for (let i = 0; i < 500 ; i++) {
        document.getElementById('iter').innerHTML = i+1;
        life_print_step();
        await sleep(5);
    }
}

const importObject = {
    life_ext: {
        height: (arg) => height(arg),
        width: (arg) => width(arg),
        sleep: (arg) => sleep(arg),
        cell_print: (arg) => cell_print(arg),
        clear_screen: (arg) => clear_screen(arg),
        newline: (arg) => newline(arg),
        rand_val: (arg) => rand_val(arg)
    },
};

WebAssembly.instantiateStreaming(fetch('life.wasm'), importObject).then(module => {
    life_print_step = module.instance.exports.life_print_step;
    life();
});
