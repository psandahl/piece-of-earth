// Fragment shader for entity.
#version 330 core

// Ambient light type.
struct AmbientLight
{
  vec3 color;
  float strength;
};

// Interpolated vertex attributes.
in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;

// The ambient light.
uniform AmbientLight ambientLight;

// Mandatory output; the color for the fragment.
out vec4 color;

vec3 baseColor();
vec3 calcAmbientLight();

void main()
{
  vec3 fragmentColor = baseColor() * calcAmbientLight();
  color = vec4(fragmentColor, 1.0);
}

// The base color for the fragment.
vec3 baseColor()
{
  return vec3(192.0 / 255.0);
}

// Calculate the ambient light for the fragment.
vec3 calcAmbientLight()
{
  return ambientLight.color * ambientLight.strength;
}
