int main(){
    int i;
    for(i = 0; i < 5; i = i + 1)
        debug i;
    while(i>1)
        i=i-1;
    debug i;
    do
    i=i+1;
    while(i<5);
    debug i;
}