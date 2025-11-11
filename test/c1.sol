contract C1 {
    int count; // this is a comment
    function f() { skip };
    function pay(?x:T,y) {
        require (count<MAX_COUNT);
        if (sender==0) { count=count+1; s="ciao" }
        else { sender!balance:T }
    }
    /*
    function g() { skip };
    function h() { skip };    
    */
}
