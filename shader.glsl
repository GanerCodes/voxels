#version 330 core
out vec4 FragColor;
in vec3 pos;

uniform sampler2D tex;
uniform     float t;
uniform      vec2 res;
uniform      vec3 face;
uniform      vec3 vp_loc;
uniform      vec2 vp_ang;
uniform      bool preview;
// uniform      bool invert;
uniform       int N;
bool invert = false;

#define PI 3.14159265359

vec3 rot_XY(vec3 p,float r){return vec3(p.x*cos(r)-p.y*sin(r),p.x*sin(r)+p.y*cos(r),p.z);}
vec3 rot_XZ(vec3 p,float r){return vec3(p.x*cos(r)-p.z*sin(r),p.y,p.x*sin(r)+p.z*cos(r));}
vec3 rot_YZ(vec3 p,float r){return vec3(p.x,p.y*cos(r)-p.z*sin(r),p.y*sin(r)+p.z*cos(r));}
vec3 rot_XZ_YZ(vec3 p, float r1, float r2) {return rot_XZ(rot_YZ(p, r1), r2);}

float sq(float x){return x*x;}
float min2(vec2 v){return min(v.x,v.y);}
float max2(vec2 v){return max(v.x,v.y);}
float min3(vec3 v){return min(min(v.x,v.y),v.z);}
float max3(vec3 v){return max(max(v.x,v.y),v.z);}

int  v2i(vec3 s) {return int(255*s.x) + 0x100*int(255*s.y) + 0x10000*int(255*s.z);}
vec3 i2v(int z) {return vec3(z&0xFF, (z>>8)&0xFF, (z>>16)&0xFF) / 255.0;}

vec3 cubecore(vec3 p) { return vec3(floor(p.x), floor(p.y), floor(p.z)); }
vec3 intocube(vec3 p) { return cubecore(N * 0.5*(p+1)); }
vec3 fromcube(vec3 p) { return p / N * 2 - 1; }

// float f_A(float x, float y, float z) { return 3 * sin(2*atan(y,x)+z) + 2*sqrt(x*x+y*y) - 0.5; }
// float f_b(float x, float y, float z) { return f_A(x + 2*cos(z), y+2*sin(z), z/3); }
// float f_A(float x, float y, float z) { return sin(atan(y,x)+3*z) + 2*sqrt(x*x+y*y) - 0.5; }
// float f_b(float x, float y, float z) { return f_A(f_A(x,y,z),f_A(y,z,x),f_A(z,x,y)) - 4; }

/* vec3  circmap(float x,float y,float z) { return vec3(sqrt(sq(x)+sq(y))-5,z,2*2*atan(y,x)); }
float       g(float x,float y,float z) { return sqrt(sq(x+sin(z))+sq(y+cos(z))) - 2.0/3; }
float      f1(float x,float y,float z) { return min3(vec3(g(x,y,z-2*PI/3), g(x,y,z), g(x,y,z+2*PI/3))); }
float      f_(float x,float y,float z) { vec3 p = circmap(x,y,z); return f1(p.x,p.y,p.z); } */

#define C 2.0
#define R 7.0
float g(vec3 p) {
    return length(vec2(p.x+sin(p.z), p.y+cos(p.z)))-1.0/3.0; }
float f2(vec3 p) {
    return min(min(
        g(p+vec3(0,0,0.0*2*PI/3.0)),
        g(p+vec3(0,0,1.0*2*PI/3.0))),
        g(p+vec3(0,0,2.0*2*PI/3.0))); }
float f_(vec3 p) {
    return f2(vec3(length(p.xy)-5.0, p.z, (1+1.0/(3*2))*2*atan(p.y,p.x))); }

// float      f_(float x,float y,float z) { return max(abs(x),max(abs(y),abs(z))) - 0.3; }

float f(vec3 p) { return max(max3(abs(p)) - (1 - min(0.01, 4.0/C)), (invert ? -1 : 1)*f_(R*p)); }

vec4 f_p(vec3 p) {
    vec3 a = fromcube(intocube(p));
    if(f(a) <= 0) return vec4(1, 1 - N/10 * length(a - p), 1,  1);
    if(max(max3(abs(p)) - 1, 0) <= 0) return vec4(1, 1, 1, .1);
    return vec4(0);
}
vec3 render(vec2 uv) {
    float FOV = 125;
    float cam_dist = 100.0;
    float hFOV = FOV / 2.0 * tan(PI * FOV / 360.0) * 2.0;
    vec3 p_e = vec3(hFOV * (uv.xy - 0.5) * (res / res.y), -cam_dist);
    
    vec3 cast_s = vp_loc;
    vec3 cast_e = cast_s + rot_XZ_YZ(p_e, -vp_ang.y, vp_ang.x);
    vec3 dir = normalize(cast_e - cast_s);
    // float d = 0.0075;
    float d = 0.0005;
    
    vec3 p = cast_s;
    if(f_p(p).w >= 0.99) invert = !invert;
    
    vec4 clr = vec4(0);
    float pre_a = 0;
    
    int MAX_ITERS = 300; // reality takes up to this + ≈ln₂(d₀ ⋅ 2ˡᵒᵈ􋕟ˡᵛˡˢ⁺¹ / hone_dist)
    bool LOD = true;
    for(int i = 0; i < MAX_ITERS; i++) {
        vec4 n = f_p(p);
        if(invert) n.xyz = 1 - n.xyz;
        if(n.w != pre_a) {
            if(n.w != 0) {
                vec4 pclr = clr;
                clr = mix(clr, n, (1 - clr.w) * n.w);
                if(clr.w >= 0.99) {
                    if(d > 0.00001) {
                        i = 0;
                        p -= d * dir;
                        d *= 0.5;
                        clr = pclr;
                        LOD = false;
                        continue;
                    }
                    clr.xyz *= 1.3 - 0.3 * length(p - cast_s);
                    break;
                }
            }
            pre_a = n.w;
        }
        if(LOD && i > 0 && i % (MAX_ITERS / 6) == 0) d *= 2;
        p += d * dir;
    }
    return clr.xyz;
}

// goes towards middle, box of [-1,1]³
vec3 scan(vec2 g, vec3 o, vec3 s) {
    g -= 0.5;
    
    int i = v2i(s);
    vec3 j = N * (o+1)/2;
    vec3 A = o.x != 0 ? vec3(j.x, g.x, g.y)
           : o.y != 0 ? vec3(g.x, j.y, g.y)
           :            vec3(g.x, g.y, j.z);
    bool c = f(fromcube(A - i*o)) <= 0;
    while(i++<N) {
        bool n = f(fromcube(A - i*o)) <= 0;
        if(!c && n) return i2v(i);
        c = n; }
    return vec3(0.0); }
void main() {
    vec2 uv = gl_FragCoord.xy / res;
    if(preview) {
        FragColor = vec4(render(uv), 1);
    }else{
        vec3 v = texture2D(tex, uv).xyz;
        FragColor = v == vec3(0.0)
                    ? vec4(vec3(0.0), 1.0)
                    : vec4(scan(gl_FragCoord.xy, face, v), 1.0);
    }
}