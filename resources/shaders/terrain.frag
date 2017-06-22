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

vec2 flipTexCoord(vec2 texCoord);

void main()
{
  vec2 texCoord = flipTexCoord(vTexCoord);
  vec3 textureColor = texture2D(groundTexture, texCoord).rgb;
  color = vec4(mix(textureColor, vColor.rgb, 0.8), 1);
}

// The texture is loaded up-side-down. The t componend must be flipped.
vec2 flipTexCoord(vec2 texCoord)
{
  float upper = ceil(texCoord.t);
  float lower = floor(texCoord.t);
  float diff = upper - texCoord.t;

  return vec2(texCoord.s, lower + diff);
}
