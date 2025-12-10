//SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

contract C1 {
    address owner;
    int x;
    bool b;

    constructor() payable { 
        owner = msg.sender;
    }

    function f1() public payable { 
        if (b && msg.sender == owner) x = x+1;
        else b=true;
    }

    function f2(address a) public {
        if (address(this).balance > 0)
            payable(a).transfer(1);
    }

    function f3(uint amt) public {
        if (address(this).balance < 8) b=false;
        else payable(msg.sender).transfer(amt);
    }
}
