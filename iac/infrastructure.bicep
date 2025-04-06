targetScope = 'subscription'

@description('Configure all linux machines with the SSH RSA public key string. Your key should include three parts, for example \'ssh-rsa AAAAB...snip...UcyupgH azureuser@linuxvm\'')
param sshRSAPublicKey string

@description('Location of the services')
param region string = 'eastus'

@description('Type of environment.')
param environment string = 'development'

resource rg 'Microsoft.Resources/resourceGroups@2021-01-01' = {
  name: 'campaigns'
  location: region 
}


module acr 'acr.bicep' = {
  scope: rg
  name: 'campaigns-acr'
  params: {
      location: region 
      acrName: 'campaignsacr'
    }
}

module aks 'aks.bicep' = {
    scope: rg
    name: 'campaings-aks'
    params: {
        location: region 
        clusterName: 'campaigns-aks'
        agentCount: 2
        agentVMSize: 'Standard_D2ps_v6' // ARM
        dnsPrefix: environment 
        linuxAdminUsername: 'campaigns'
        sshRSAPublicKey: sshRSAPublicKey 
        osDiskSizeGB: 0
        acrName: 'campaignsacr' 
    }
}
