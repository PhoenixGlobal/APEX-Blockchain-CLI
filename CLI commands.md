
## Command grouping

- Sys
- Chain
- Wallet
- Account
- Asset
- Contract
- Producer
- Proposal

### 1. sys
#### Command Line Interface to the system, omit it and type the sub command directly is legal.

1. Display version information  
<code>sys version </code>

 * You can also use shorthand.  
 <code>sys ver</code>


2. Get help information for the command grouping  
<code>sys help</code>

 * get help information for the specified command  
 
 Get help information for version commands under sys grouping  
 
 <code>sys version -h</code>   
 
 Get help information for block commands under chain grouping  
 
 <code>chain block -h</code>
 


3. Exit command-line tool  
<code>sys exit</code>


4. Clear characters on screen   
<code>sys clear</code>

### 2. chain
#### Command Line Interface to the block chain, omit it and type the sub command directly is legal.

1. Display the status of the block chain   

  <code>chain status</code>
  
  
2. Query block data   

  * Query blocks by block hash
  
    <code>chain block -hash 09201caffdc2924beaae1b82bdc179919dfa9d31e37b9b6c484cd273645d07de</code>  
    
    <code>block -hash 09201caffdc2924beaae1b82bdc179919dfa9d31e37b9b6c484cd273645d07de</code>
    
  
  * Query blocks by block height
  
    <code>chain block -height 34240</code>  
    
    <code>block -height 34240</code>
  
  
  
3. Query transaction data   

  * Query transaction by transaction hash
  
     <code>chain tx -hash 09201caffdc2924beaae1b82bdc179919dfa9d31e37b9b6c484cd273645d07de</code>    
     
     
     
4. Query the balance of an address, nonce information
  
  <code>chain account -addr AP8hKwPm8X37YnsDn47KHF7JzvRVUuMD7YH</code>
  
  
5. Query the average gasPrice for the latest 1000 intra-block transactions across the network

  <code>chain gas</code>
  
6. Private key format conversion, which can convert one private key format to a private key in another format

  <code>chain key -input KzjVsCDaRAGzhykx3hCZT5L7e9F5pa2jVHL7SR2C4YGqvus8X1e3</code>
  
7. Create tx tool

	<code>chain createtx -key KyAa97eFf7koU4pN62fYQQDDq474varenc9eQtc1R9X27UTnjAQ5 -type 1 -from APHLU98wbCvujZc1N8ddthDNEUhJaxki2sz -to APNvjvtwsxKuQak7P9ssZhgzcY3dTJDAsVg  -amount 100000000000000000000 -nonce 2 -data 0 -gasprice 1 -gaslimit 9000000 -executetime 0 -version 1</code>
  
8. tx sign tool (-key XXX -data XXX)

	<code>chain sign -key KyAa97eFf7koU4pN62fYQQDDq474varenc9eQtc1R9X27UTnjAQ5 -data 0000000101a8b14d178a255a7bf9b8d14fc85ac043fe982adae60540b15ad165489ac4624ee65b7e79cf403d3309056bc75e2d6310000000000000000000000001010400895440000000000000000045304302201d53b653d69d83e1cd8342148dd67b156dd38b697ebae03cc7213cd3b83e1703021f7b27e8ed192bb7000e3745c667bda1b69d8cdef7c303186753e510b54d4f77

### 3.wallet
#### Operate a wallet, user accounts must add to one wallet before using it

1. View local wallet information  

  <code>wallet list</code>
  
  
2. Create a wallet named APEX1  

  <code>wallet new -n APEX1</code>
  
  Then enter the password to create it successfully
  
  
  
3. Activate wallet

  The account in the wallet can only be operated when the wallet is active.
 
  <code>wallet activate -n APEX1 -p</code>
  
  <code>wallet act -n APEX1</code>
  
  Then enter the password for this wallet
  
  
  
4. Close the wallet

  Closing the wallet is to remove the activation of the wallet.  
  
  <code>wallet close </code>

### 4.account
#### Operate accounts of current wallet

1. List all accounts of current wallet

  <code>account list</code>  
  

2. Create an account named Vitalik1 

  <code>account new -a Vitalik1</code>
  
  Note that numbers must exist in the account name  
  
  
3. Import an account through a private key,named Vitalik

  <code>account import -key L5F9wynVAwc6d5AXVkdmW1G4CVLJXV5vp5keF91fx2FPsG7Tzpyv -a Vitalik1</code>  
  
  
4. Export private key information for an account

  <code>account export -a Vitalik1</code>  
  
  
5. Delete one account from current wallet

  <code>account delete -a Vitalik1</code>
  
  <code>account remove -a Vitalik1</code>  
  
  
6. Rename an account,rename the account named Vitalik1 to Bytemaster1

  <code>account rename -a Vitalik1 -to Bytemaster1</code>
  
  
7. Show the status of account  

  <code>account show -a Bytemaster1</code> 
  
  
8. Set the default account for the wallet
  
  <code>account imply -a Bytemaster1</code>
  
  
9. View the current nonce of the account

  <code>account getNonce -a Bytemaster1</code>
  
  <code>account getNonce -addr APK6FBosUGNqAXAsbJCi2kN5AMbgsjmvhrs</code>

### 5.Asset
#### Interface to operate your funds,  omit it and type the sub command directly is legal.

1. Transfer tokens  

   <code>asset send -from Bytemaster1 -to AP3CFsBWwVMsF2Kq39nZ4XBYZcoR1HQhKru -amount 30 -gasLimit 300000 -gasPrice 30k -nonce 23</code>   

  Parameter interpretation  
  
  * from,Can be omitted, omitted to use the default account  
  
  * to,Transfer destination address   
  
  * amount,CPX amount transferred   
  
  * gasLimit,gas ceiling for acceptable consumption in this transfer  
  
  * gasPrice,gas price willing to pay for this transfer.The unit is k, m, g, kg, mg
  
  * Optional parameter, the Nonce value used by this transaction signature
 
  
   Optional parameter, the Nonce value used by this transaction signature

   ![avatar](unit.png)
   
   Gas fee calculation case：   
   
   Suppose a transfer costs 21000 gas, and the gasPrice filled in at the time of the transfer is 10G，   
   Then the handling fee for the cost of this transfer is:  
   
   21000*10^(-9) CPX


2. Broadcast original transaction information  

   <code>asset broadcast -data F:\desk\TXT.txt</code>



### 6.Contract
#### Operate smart contract

1. Compile smart contract  

  <code>contract compile  -s E:\sdaf.sol -n contractName -p E:\contractFilePath </code>
  
  Parameter interpretation   
  * s:Contract file location
  * n:The name of the contract to compile
  * p:Contract compiled output file path
  
  
2. Deploy smart contracts  

  <code>contract run -p deploy(gasLimit, gasPrice, amount, "/path/to/contract.sol",  contractName,[parameters1,parameters2....])</code>
  
   Parameter interpretation   
   
   In the deploy method, enter the gasLimit,gasPrice,amout, contract file path in turn, the name of the contract to deploy, and the input parameters for this contract.
   
   If contract has constructor parameters, pending at last.
   
   If parameter is a out function, parameter format should be like: signFunction(contactAddress,functionName,functionArgsType1,functionArgsType2...)
   

   
    
    
3. Call Smart contract  

  <code>Contract run -p call(gasLimit,gasPrice,amount, "APxxxx", "/path/to/contract.abi",  
    methodName,parameters1,parameters2....)</code>
    
   The contract is invoked in a similar way to the deployment contract, and the input file that invokes the contract can use either the contract code or the contract's ABI file
    
    

### 7.Producer

#### Some commands related to production nodes

1. Registered alternative producer

  <code>producer reg -from Vitalik1 -url www.test.com -country china -address 浦江金融国际广场 -longitude 20 -latitude 40 -company APEX_NETWORK -gasPrice 300g</code>
  
   Parameter interpretation   
   * from:the account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.
   
   * url: the offical website.
   
   * country: the country where the production node is located.
   
   * company：the country where the production node is located.
   
   * address：the contact address of the producer node,which is used to determind the order of all producers
   
   * longitude：the position of the production node in the whole network is used to determine the working order of all producers.
   
   * latitude: the position of the production node in the whole network is used to determine the working order of all producers.
   
   * gasPrice: the price of gas that the transaction is willing to pay.
   
  
  

2. Cancellation of alternative producer qualifications  

  <code>producer unReg -from Bytemaster1 -gasPrice 30k</code>  
    
    
3. Vote for supported producers

  <code>producer vote -from Bytemaster1 -addr AP8AP8iUQsnpbT73LkpBS8oNzSRGJAvBoVE - amount 10 -gasPrice 30k</code>
  
  
4. Enquire about callable votes

   <code>producer getVote -addr APMMCd8qWPm9QRzgspFXEBn8zGGuwrYYAJs</code>
   

5. Redemption vote  

   <code>producer unVote -from Bytemaster1 -addr AP8AP8iUQsnpbT73LkpBS8oNzSRGJAvBoVE - amount 10  -gasPrice 30k</code>


6. Query producer information

  <code>producer list -type all</code>

   Supports four query types,   
   
   all:query all producer information;  
   
   Previous:query the last round of production nodes;   
   
   act:Query the working production node;   
   
   pending:Query the next round of production nodes with production rights.
   
   
7. Querying producer information by node address

  <code>producer getByAddr -addr AP8AP8iUQsnpbT73LkpBS8oNzSRGJAvBoVE</code>
  


### 8.Proposal

#### The relevant orders of the main network proposal shall be used to initiate a proposal to modify the block consensus parameters.

1. Initiate a proposal  

  <code>proposal new -from Bytemaster1 -para 1 -time 1557297681 -content 13 gasPrice 30k</code>
  
   Parameter interpretation:
  
   - from:The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.  
   - para:The parameter you proposal to change. (1:BlockAward, 2:TxMinGasPrice, 3:TxMaxGasLimit)
   - time:The proposal apply time (UTC seconds)   
   - content: The proposal data.   
   - gasPrice:The price of gas that the transaction / contract is willing to pay.
  
  
  
2. Support / reject a proposal

  <code>proposal vote -from Bytemaster1 -pid dfeb09a8dc986881edaa9441187a8349679dd9251bb589fc7c312277fbc5c5a1 -agree 1 -gasPrice 39k</code>
   
   Parameter interpretation:  
   - from:The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.   
   - pid:The proposal ID/TXID    
   - agree: 1 -> agree,   0 -> disagree     
   - gasPrice: The price of gas that the transaction / contract is willing to pay.



3. List the voting status of all current proposals

  <code>proposal voteList</code>


4. List all current proposals

  <code>proposal list</code>






