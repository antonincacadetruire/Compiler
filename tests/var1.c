int i(int i){
    return i-2;
}
int main(){
    int i;
    i = 0;
    {
        int i;
        i = 1;
        {
            int i;
            i = 2;
            debug i;
        }
        debug i(i);
    }
    debug i;
}
