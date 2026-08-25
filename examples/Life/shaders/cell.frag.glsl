varying vec2 vTextureCoord;
uniform sampler2D uSampler;
uniform sampler2D uAtlas;
uniform sampler2D uPrevAtlas;
uniform vec2 uTexSize;
uniform vec4 inputSize;
uniform vec2 uPan;
uniform vec3 uBg;
uniform float uCellPx;

vec4 sc(vec2 id, float dx, float dy) {
  vec2 nid = id + vec2(dx, dy);
  if (nid.x < 0.0 || nid.y < 0.0 || nid.x >= uTexSize.x || nid.y >= uTexSize.y) {
    return vec4(0.0);
  }
  vec2 nuv = (nid + 0.5) / uTexSize;
  return mix(texture2D(uPrevAtlas, nuv), texture2D(uAtlas, nuv), 0.5);
}

vec4 mc(vec2 world, vec2 id, vec3 refC, float dx, float dy) {
  vec4 nc = sc(id, dx, dy);
  if (nc.a < 0.04) {
    return vec4(0.0);
  }
  vec2 delta = world - (id + vec2(dx, dy) + 0.5);
  float r2 = dot(delta, delta);
  float R = 0.56;
  float meta = (R * R) / (r2 + R * R * 0.09);
  float same = exp(-length(nc.rgb - refC) * 11.0);
  float w = meta * mix(0.08, 1.0, same);
  return vec4(nc.rgb * w, w);
}

vec3 refHue(vec2 id, vec4 center) {
  if (center.a >= 0.04) {
    return center.rgb;
  }
  vec3 rgb = vec3(0.0);
  float wt = 0.0;
  vec4 n;
  n = sc(id, -1.0, -1.0);
  if (n.a >= 0.04) {
    rgb += n.rgb;
    wt += 1.0;
  }
  n = sc(id, 0.0, -1.0);
  if (n.a >= 0.04) {
    rgb += n.rgb;
    wt += 1.0;
  }
  n = sc(id, 1.0, -1.0);
  if (n.a >= 0.04) {
    rgb += n.rgb;
    wt += 1.0;
  }
  n = sc(id, -1.0, 0.0);
  if (n.a >= 0.04) {
    rgb += n.rgb;
    wt += 1.0;
  }
  n = sc(id, 1.0, 0.0);
  if (n.a >= 0.04) {
    rgb += n.rgb;
    wt += 1.0;
  }
  n = sc(id, -1.0, 1.0);
  if (n.a >= 0.04) {
    rgb += n.rgb;
    wt += 1.0;
  }
  n = sc(id, 0.0, 1.0);
  if (n.a >= 0.04) {
    rgb += n.rgb;
    wt += 1.0;
  }
  n = sc(id, 1.0, 1.0);
  if (n.a >= 0.04) {
    rgb += n.rgb;
    wt += 1.0;
  }
  return wt > 0.0 ? rgb / wt : center.rgb;
}

void main(void) {
  vec2 screen = vTextureCoord * inputSize.xy;
  float s = max(uCellPx, 0.001);
  vec2 world = (screen - uPan) / s;
  if (world.x < 0.0 || world.y < 0.0 || world.x >= uTexSize.x || world.y >= uTexSize.y) {
    gl_FragColor = vec4(uBg, 1.0);
    return;
  }
  vec2 id = floor(world);
  vec2 f = fract(world);
  vec2 p = f - 0.5;
  vec4 center = sc(id, 0.0, 0.0);
  vec3 refC = refHue(id, center);
  vec4 acc = vec4(0.0);
  acc += mc(world, id, refC, -3.0, -3.0);
  acc += mc(world, id, refC, -2.0, -3.0);
  acc += mc(world, id, refC, -1.0, -3.0);
  acc += mc(world, id, refC, 0.0, -3.0);
  acc += mc(world, id, refC, 1.0, -3.0);
  acc += mc(world, id, refC, 2.0, -3.0);
  acc += mc(world, id, refC, 3.0, -3.0);
  acc += mc(world, id, refC, -3.0, -2.0);
  acc += mc(world, id, refC, -2.0, -2.0);
  acc += mc(world, id, refC, -1.0, -2.0);
  acc += mc(world, id, refC, 0.0, -2.0);
  acc += mc(world, id, refC, 1.0, -2.0);
  acc += mc(world, id, refC, 2.0, -2.0);
  acc += mc(world, id, refC, 3.0, -2.0);
  acc += mc(world, id, refC, -3.0, -1.0);
  acc += mc(world, id, refC, -2.0, -1.0);
  acc += mc(world, id, refC, -1.0, -1.0);
  acc += mc(world, id, refC, 0.0, -1.0);
  acc += mc(world, id, refC, 1.0, -1.0);
  acc += mc(world, id, refC, 2.0, -1.0);
  acc += mc(world, id, refC, 3.0, -1.0);
  acc += mc(world, id, refC, -3.0, 0.0);
  acc += mc(world, id, refC, -2.0, 0.0);
  acc += mc(world, id, refC, -1.0, 0.0);
  acc += mc(world, id, refC, 0.0, 0.0);
  acc += mc(world, id, refC, 1.0, 0.0);
  acc += mc(world, id, refC, 2.0, 0.0);
  acc += mc(world, id, refC, 3.0, 0.0);
  acc += mc(world, id, refC, -3.0, 1.0);
  acc += mc(world, id, refC, -2.0, 1.0);
  acc += mc(world, id, refC, -1.0, 1.0);
  acc += mc(world, id, refC, 0.0, 1.0);
  acc += mc(world, id, refC, 1.0, 1.0);
  acc += mc(world, id, refC, 2.0, 1.0);
  acc += mc(world, id, refC, 3.0, 1.0);
  acc += mc(world, id, refC, -3.0, 2.0);
  acc += mc(world, id, refC, -2.0, 2.0);
  acc += mc(world, id, refC, -1.0, 2.0);
  acc += mc(world, id, refC, 0.0, 2.0);
  acc += mc(world, id, refC, 1.0, 2.0);
  acc += mc(world, id, refC, 2.0, 2.0);
  acc += mc(world, id, refC, 3.0, 2.0);
  acc += mc(world, id, refC, -3.0, 3.0);
  acc += mc(world, id, refC, -2.0, 3.0);
  acc += mc(world, id, refC, -1.0, 3.0);
  acc += mc(world, id, refC, 0.0, 3.0);
  acc += mc(world, id, refC, 1.0, 3.0);
  acc += mc(world, id, refC, 2.0, 3.0);
  acc += mc(world, id, refC, 3.0, 3.0);
  float field = acc.a;
  vec3 fluid = acc.rgb / max(field, 0.001);
  float body = smoothstep(0.90, 1.18, field);
  float live = step(0.04, center.a);
  float core = exp(-dot(p, p) * 15.0) * live;
  float corona = pow(clamp(field * 0.72, 0.0, 1.0), 2.8) * (1.0 - body * 0.80);
  vec3 rgb = mix(uBg, fluid * (0.62 + 0.38 * body), body);
  rgb += fluid * corona * 0.95;
  rgb += fluid * core * 0.55;
  gl_FragColor = vec4(clamp(rgb, 0.0, 1.0), 1.0);
}
