//SPDX-License-Identifier: UNLICENSED
pragma solidity >= 0.8.2;

contract EtherStore {
    mapping(address => uint) public balances;

    function deposit() public payable {
        balances[msg.sender] += msg.value;
    }

    function withdraw() public {
        uint bal; 
        bal = balances[msg.sender];
        require(bal > 0);

        payable(msg.sender).transfer(bal);
        balances[msg.sender] = 0;
    }
}
