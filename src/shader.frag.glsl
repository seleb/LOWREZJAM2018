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
uniform vec2 mouse;

const vec2 size = vec2(64,64);
const float diag = sqrt(size.x*size.y*2.0);
const float inc = 0.1;

const float blend = 0.69;
const float coneTightness = 0.69;
const float coneSafety = 0.42;

float rand(vec2 co){
	return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453)*2.0 - 1.0;
}

void main(){
	vec2 coord = gl_FragCoord.xy;
	vec2 uv = coord.xy / resolution.xy;
	vec2 source = player * size;

	vec2 r = vec2(
		rand(vec2(uv.x+time/200.0, uv.y+time/500.0)) / 2.0,
		rand(vec2(uv.y+time/400.0, uv.x+time/300.0)) / 2.0
	);

	float flashAnim = sin(time/60.0)*0.005+sin(time/20.0)*0.003+rand(uv+vec2(mod(time/100.0,1.0),mod(time/200.0,1.0)))*0.008;

	vec2 a = coord - source;
	vec2 a2 = mouse*size - source;
	float cone = (dot(a, a2) / (length(a)*length(a2)));
	cone += sin(time/100.0)*0.003 + sin(time/20.0)*0.002;
	cone = smoothstep(coneTightness, 1.0, cone);

	vec4 tex = texture2D(tex0,uv);

	float light = 1.0;
	float wall = 1.0;
	float steps = distance(source, coord) / diag;

	for(float i = 0.0; i <= diag; i += inc) {
		vec2 p = mix(source + r, coord, i/diag);
		
		vec3 sample = texture2D(tex0, p/resolution.xy).rgb;
		wall = min(wall, sample.r);
		float bleed = sample.g;
		light = mix(light, wall, (1.0 - bleed) * steps);
	}
	light = clamp(light, 0.0, 1.0);
	

	float mult = pow(255.0, mix(tex.b*2.0, (tex.b-0.5)*2.0+1.0, step(0.5, tex.b)))/255.0;
	float attenuation = pow(clamp(1.0 - distance(source, coord) / (diag * 0.5), 0.0, 1.0), 2.0);
	attenuation += rand(vec2(mod(time,1000.0),light))*0.01;

	float coneMult = mix(1.0, cone, smoothstep(1.0, 1.0-(coneSafety+flashAnim), attenuation));

	float diffuse = light * mult * attenuation * coneMult;

	vec3 col = vec3(diffuse);
	
	col = mix(col, texture2D(tex1, uv).rgb, blend);

	// alpha -> white (for text)
	col = mix(tex.rgb, col, ceil(tex.a-1.0/255.0));

	gl_FragColor = vec4(vec3(col), 1.0);
}
