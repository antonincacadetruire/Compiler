int malloc(int n){
    int r;
    r = *0;
    *0 = *0 + n;
    return r;
}

int free(int p){
}

int printrec(int n){
    int up;
    up = n/10;
    if(up > 0){
        printrec(up);
        send (n%10)+48;
    }
    else {
        send n+48;
    }
}

int print(int n){
    printrec(n);
    send 10;
}

int read(){
    int a;
    a = 48;
    int result;
    result = 0;
    int compteur;
    compteur = 10;
    while(a != 10){
        a = recv;
        if(a>=48 && a <58){
            result = result*compteur+(a-48);
        }
        
    }
    return result;
}