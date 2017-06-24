// Fragment shader for terrain.
#version 330 core

// Ambient light type for the terrain.
struct AmbientLight
{
  vec3 color;
  float strength;
};

// Light emitter type for the terrain.
struct LightEmitter
{
  vec3 position;
  vec3 color;
  float strength;
};

// Interpolated texture coordinates.
in vec2 vTexCoord;

// Interpolated vertex color.
in vec4 vColor;

// The ground texture for all terrain.
uniform sampler2D groundTexture;

// The ambient light.
uniform AmbientLight ambientLight;

// The sun light.
uniform LightEmitter sunLight;

// Mandatory; the color for the fragment.
out vec4 color;

vec3 baseColor();

void main()
{
  vec3 ambientColor = ambientLight.color * ambientLight.strength;
  vec3 fragmentColor = baseColor() + ambientColor;
  color = vec4(fragmentColor, 1);
}

// Calculate the base color from the color map and the vertex' color. I.e. the
// color before any lightning is applied.
vec3 baseColor()
{
  vec3 textureColor = texture2D(groundTexture, vTexCoord).rgb;
  return mix(textureColor, vColor.rgb, 0.8);
}
