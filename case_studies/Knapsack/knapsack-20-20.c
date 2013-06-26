#define N 20 
#define CAP 20 
#define CAP1 CAP+1
#define N1 N+1


int main(){

  unsigned int value[N];
  unsigned int weight[N];
  unsigned int C = CAP;
  unsigned int n=N;

  unsigned int m[128][128];
  unsigned int i,j;
  
  for( j = 0; j <= C; j++){
    m[0][j] = 0;
  }

  for (i = 1; i <= n; i++){
    for (j = 0; j <=C; j++){
      if (j >= weight[i-1]){
        if (m[i-1][j] > m[i-1][j-weight[i-1]] + value[i-1]){
          m[i][j] = m[i-1][j];
        }else{
          m[i][j] = m[i-1][j-weight[i-1]] + value[i-1];
        }
      }
      else{
        m[i][j] = m[i-1][j];
      }
    }
  }

  return 0;
}
