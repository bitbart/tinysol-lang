// unit testing bank.sol
faucet 0xA 100
faucet 0xB 100
deploy 0xA:0xC() "test/contracts/Bank.sol"

0xA:0xC.deposit{value:20}()
assert 0xA this.balance==80
assert 0xB this.balance==100
assert 0xC this.balance==20

0xB:0xC.deposit{value:30}()
assert 0xA this.balance==80
assert 0xB this.balance==70
assert 0xC this.balance==50

revert 0xA:0xC.withdraw(21)

0xA:0xC.withdraw(15)
assert 0xA this.balance==95
assert 0xB this.balance==70
assert 0xC this.balance==35

0xB:0xC.withdraw(30)
assert 0xA this.balance==95
assert 0xB this.balance==100
assert 0xC this.balance==5
