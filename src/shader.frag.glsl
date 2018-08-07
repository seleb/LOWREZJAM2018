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

uniform float on;
uniform float coneTightness;

const vec2 size = vec2(64.0);
const float diag = sqrt(size.x*size.y*2.0);
const float inc = 0.1;

const float blend = 0.69;
const float coneSafety = 0.42;

// https://gist.github.com/patriciogonzalezvivo/670c22f3966e662d2f83
//	Simplex 3D Noise 
//	by Ian McEwan, Ashima Arts
//
vec4 permute(vec4 x){return mod(((x*34.0)+1.0)*x, 289.0);}
vec4 taylorInvSqrt(vec4 r){return 1.79284291400159 - 0.85373472095314 * r;}

float noise(vec3 v){ 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //  x0 = x0 - 0. + 0.0 * C 
  vec3 x1 = x0 - i1 + 1.0 * C.xxx;
  vec3 x2 = x0 - i2 + 2.0 * C.xxx;
  vec3 x3 = x0 - 1. + 3.0 * C.xxx;

// Permutations
  i = mod(i, 289.0 ); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients
// ( N*N points uniformly over a square, mapped onto an octahedron.)
  float n_ = 1.0/7.0; // N=7
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z *ns.z);  //  mod(p,N*N)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
}

// https://stackoverflow.com/questions/12964279/whats-the-origin-of-this-glsl-rand-one-liner
float rand(vec2 co){
	return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453)*2.0 - 1.0;
}

//http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
vec3 rgb2hsv(vec3 c){
	vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
	vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
	vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

	float d = q.x - min(q.w, q.y);
	float e = 1.0e-10;
	return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec3 hsv2rgb(vec3 c){
	vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
	vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
	return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
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
	float cone = dot(normalize(a), normalize(a2));
	cone += sin(time/100.0)*0.003 + sin(time/20.0)*0.002;
	cone = clamp(0.0, 1.0, cone);
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
	attenuation += noise(vec3(distance(source, coord)/diag, sin(time/1000.0), cos(time/2000.0))) * 0.02;
	attenuation += rand(vec2(mod(time,1000.0),light))*0.01;
	attenuation *= 1.0 - floor(rand(vec2(mod(time,10000.0), 0.0))/2.0 + 0.501) - floor(rand(vec2(mod(time,3000.0), 0.0)) + 1.0)*0.05;

	float coneS = smoothstep(1.0, 1.0-(coneSafety+flashAnim), attenuation);
	float coneMult = mix(1.0, cone, coneS);

	// final lighting calc
	float diffuse = light * mult * attenuation * coneMult;

	// swirl color calc
	vec2 swirl = coord;
	swirl -= source;
	swirl *= 2.0;
	float d = length(swirl);
	d /= diag;
	d += sin(uv.x*d + time / 200.0) * 0.1;
	d += sin(uv.y*d + time / 300.0) * 0.1;
	d -= time / 1000.0;
	float s = sin(d * 10.0 + time/2000.0);
	float c = sin(d * 10.0 + time/3000.0);
	vec2 nuv = uv;
	nuv *= vec2(
		dot(swirl, vec2(c, -s)),
		dot(swirl, vec2(s, c))
	) / diag;
	nuv += vec2(
		sin(nuv.x * 2.0 + time/400.0),
		cos(nuv.y * 2.0 + time/500.0)
	);
	float n = noise(vec3(nuv.x + time/2000.0, nuv.y + time/3000.0, time/1000.0));
	n += n*n;

	vec3 c1 = rgb2hsv(vec3(210.0, 75.0, 108.0)/256.0);
	c1.z = 1.0;
	vec3 c2 = rgb2hsv(vec3(82.0, 171.0, 201.0)/256.0);
	c2.z = 1.0;
	vec3 col = hsv2rgb(vec3(mix(c1, c2, n)));

	col = mix(vec3(diffuse), col*diffuse, (coneMult-attenuation+diffuse)*on);
	
	// blend
	col = mix(col, texture2D(tex1, uv).rgb, blend);

	// alpha -> white (for text)
	col = mix(tex.rgb, col, ceil(tex.a-1.0/255.0));

	gl_FragColor = vec4(vec3(col), 1.0);
}
