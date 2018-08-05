// 2d raycast lighting fragment shader
// r = darkness of shadow cast (0 = full shadow, 255 = no shadow)
// g = translucency (i.e. how much light bleeds through) (0 = solid, 255 = transparent)
// b = brightness multiplier (0 = black, 128=same, 255 = white)
// a = global mix (0 = none, 255 = all)
precision mediump float;

uniform sampler2D tex0;
uniform sampler2D tex1;
uniform float time;
uniform vec2 resolution;
uniform vec2 player;

const vec2 size = vec2(64,64);
const float diag = sqrt(size.x*size.y*2.0);
const float inc = 0.1;

float rand(vec2 co){
	return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

void main(){
	vec2 coord = gl_FragCoord.xy;
	vec2 uv = coord.xy / resolution.xy;
	vec2 source = player * size;

	vec4 tex = texture2D(tex0,uv);

	float light = 1.0;
	float wall = 1.0;
	float steps = distance(source, coord) / diag;

	for(float i = 0.0; i <= diag; i += inc) {
		vec2 p = mix(source, coord, i/diag);
		
		vec3 sample = texture2D(tex0, p/resolution.xy).rgb;
		wall = min(wall, sample.r);
		float bleed = sample.g;
		light = mix(light, wall, (1.0 - bleed) * steps);
	}
	light = clamp(light, 0.0, 1.0);
	
	float mult = pow(255.0, mix(tex.b*2.0, (tex.b-0.5)*2.0+1.0, step(0.5, tex.b)))/255.0;
	float attenuation = pow(clamp(1.0 - distance(source, coord) / (diag * 0.5), 0.0, 1.0), 2.0);
	float diffuse = light * mult * attenuation;

	vec3 col = vec3(diffuse);
	
	const float blend = .69;
	col = mix(col, texture2D(tex1, uv).rgb, blend);

	// alpha -> white (for text)
	col = mix(tex.rgb, col, ceil(tex.a-1.0/255.0));

	gl_FragColor = vec4(vec3(col), 1.0);
}
