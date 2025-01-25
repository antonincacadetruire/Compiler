int main(){
    int i;
    for(i = 0; i < 5; i = i + 1){
        int j;
        for(j = 0; j < 5; j = j + 1){
            if(j > 2){
                break;
            }
            debug i*5+j;
        }
        if(i > 2){
            break;
        }
    }
}