// Fragment shader for terrain.
#version 330 core

// Ambient light for the terrain.
struct AmbientLight
{
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

// Mandatory; the color for the fragment.
out vec4 color;

vec2 flipTexCoord();
vec3 baseColor();

void main()
{
  color = vec4(baseColor(), 1);
}

// The texture is loaded up-side-down. The t componend must be flipped.
vec2 flipTexCoord()
{
  float upper = ceil(vTexCoord.t);
  float lower = floor(vTexCoord.t);
  float diff = upper - vTexCoord.t;

  return vec2(vTexCoord.s, lower + diff);
}

// Calculate the base color from the color map and the vertex' color.
vec3 baseColor()
{
  vec3 textureColor = texture2D(groundTexture, flipTexCoord()).rgb;
  return mix(textureColor, vColor.rgb, 0.8);
}
