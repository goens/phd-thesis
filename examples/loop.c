char* read_file(int);
int contains_cats(char*);

#define N 10
int main(){
int results[N];
int i;
  for(i = 0; i < N; i++){
    char *f;
    f = read_file(i);
    results[i] = contains_cats(f); 
}
}
