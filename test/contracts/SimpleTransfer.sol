// SPDX-License-Identifier: GPL-3.0
// pragma solidity ^0.8.1;

contract SimpleTransfer {

    address recipient;
    address owner;

    constructor(address _recipient){
        recipient = _recipient;
        owner = msg.sender;
    }

    function deposit() public payable {
        require(msg.sender == owner);
    }

    function withdraw(int amount) public {
        require(msg.sender == recipient); // only the recipient can withdraw
        require(amount <= address(this).balance - 1); // the contract balance is less then required amount

        recipient.transfer(amount);
    }

}