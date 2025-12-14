faucet 0xA 100
faucet 0xM 1   // Adversary

deploy 0xA:0xC() "test/contracts/EtherStore.sol"
deploy 0xA:0xD() "test/contracts/EtherStoreAttack.sol"

0xA:0xC.deposit{value:100}()
assert 0xA this.balance==0
assert 0xC this.balance==100

0xM:0xC.deposit{value:1}()
assert 0xA this.balance==0
assert 0xM this.balance==0
assert 0xC this.balance==101

0xM:0xC.withdraw()
assert 0xA this.balance==0
assert 0xM this.balance==101
assert 0xC this.balance==0
