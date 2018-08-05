const fs = require('fs');

const gamedata = fs.readFileSync('./src/gamedata.txt', 'utf8');
const shader = fs.readFileSync('./src/shader.frag.glsl', 'utf8');
const template = fs.readFileSync('./src/template.html', 'utf8');

fs.writeFileSync('./dist/index.html', template
	.replace('//GAMEDATA', gamedata)
	.replace('//SHADER', shader));
