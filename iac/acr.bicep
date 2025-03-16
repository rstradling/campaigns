param location string
param acrName string
param skuName string = 'Standard'

resource acr 'Microsoft.ContainerRegistry/registries@2021-09-01' = {
  name: acrName
  location: location
  sku: {
    name: skuName
  }
  properties: {
    adminUserEnabled: false
    publicNetworkAccess: 'Enabled'
  }
}

output acrId string = acr.id
output acrLoginServer string = acr.properties.loginServer
