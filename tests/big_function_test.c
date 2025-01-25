int bubbleSort(int arr, int n) {
    int i;
    int j;
    int temp;
    for (i = 0; i < n-1; i=i+1) {
        for (j = 0; j < n-i-1; j=j+1) {
            if (arr[j] > arr[j+1]) {
                temp = arr[j];
                arr[j] = arr[j+1];
                arr[j+1] = temp;
            }
        }
    }
}


int processArray(int n) {
    int *arr;
    arr = malloc(n);
    int i;
    for (i = 0; i < n; i=i+1) {
        arr[i] = n-i;
    }

    bubbleSort(arr, n);

    for (i = 0; i < n; i=i+1) {
        print(arr[i]);
    }
    free(arr);
}

int main() {
    int n;
    n = 64;
    processArray(n);

    return 0;
}