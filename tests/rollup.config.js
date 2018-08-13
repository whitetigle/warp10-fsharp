import * as path from 'path';
import fable from 'rollup-plugin-fable';

function resolve(filePath) {
    return path.join(__dirname, filePath)
}

export default {
    input: resolve('./Warp10.Tests.fsproj'),
    output: {
        file: resolve('./bin/tests.bundle.js'),
        format: 'cjs', // 'amd', 'cjs', 'es', 'iife', 'umd',
        name: 'FableREPL'
    },
    plugins: [
        fable({})
    ]
};
