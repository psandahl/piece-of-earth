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

// Interpolated vertex attributes.
in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;
in vec4 vColor;

// The ground texture for all terrain.
uniform sampler2D groundTexture;

// The view matrix. For transforming the sun light position to view space.
uniform mat4 viewMatrix;

// The ambient light.
uniform AmbientLight ambientLight;

// The sun light.
uniform LightEmitter sunLight;

// Mandatory output; the color for the fragment.
out vec4 color;

vec3 baseColor();
vec3 calcAmbientColor();
vec3 calcSunLight();

void main()
{
  vec3 ambientColor = calcAmbientColor();
  vec3 sunColor = calcSunLight();
  vec3 fragmentColor = baseColor() * (ambientColor + sunColor);
  color = vec4(fragmentColor, 1);
}

// Calculate the base color from the color map and the vertex' color. I.e. the
// color before any lightning is applied.
vec3 baseColor()
{
  vec3 textureColor = texture2D(groundTexture, vTexCoord).rgb;
  return mix(textureColor, vColor.rgb, 0.8);
}

// Calculate the ambient color for the fragment.
vec3 calcAmbientColor()
{
  return ambientLight.color * ambientLight.strength;
}

// Calculate the sun (diffuse) light. This lightning is performed in view space.
vec3 calcSunLight()
{
  vec3 normal = normalize(vNormal);
  //vec3 sunSpot = (viewMatrix * vec4(sunLight.position, 1)).xyz;
  vec3 direction = normalize(vPosition - sunLight.position);
  float diffuse = max(dot(normal, -direction), 0);

  return sunLight.color * diffuse;
}
