#include<stdio.h>
#include<stdlib.h>
#include<string.h>

//				small functions
int* binary(int a);
long power(int a,int b);
long dec(int* a);
int* mult(int* a, int* b);		// poly mult
int* mod(int* a,int* b);		// mod in the field with the gen poly

//				needed functions	
int* field_mult(int* a,int* b);
int* field_inv(int* a);
int** key_expansion(char* key);
int** add_round_key(int** key,int** s);      
int* matrix_mul(int** a,int* b1);
int* subbytes(int* z);
int* inv_subbytes(int* b);
int** inv_mix_column(int** s);
int** inv_shift_rows(int** a);


int* binary(int a){
  int *ptr,a_new;
  ptr=(int*)malloc(15*sizeof(int));
  for(int i=0;i<15;i++){
    a_new=a/2;  
    ptr[i]=a-(2*a_new);
    a=a_new;
    }  
  return ptr;
  }   
  
  
long power(int a,int b){
  long count=1;
  for(int i=0;i<b;i++)
    count=count*a;
  return count;  
  }
    

long dec(int* a){
  long count=0;
  for(int i=0;i<15;i++){
    count=count+(a[i]*power(2,i));
    }
  return count;
  }      
  
  
int* mult(int* a, int* b){
  int *c;
  c=(int*)malloc(15*sizeof(int));  
  for(int k=0;k<=14;k++){
    c[k]=0;
    for(int i=0;i<15;i++)
      for(int j=0;j<15;j++)
        if(i+j==k){
          c[k]=(a[i]*b[j])+c[k];
          c[k]=c[k]%2;
          }
    }
  return c;
  }           
  
  
int deg(int* a){
  int deg_a=0;
  for(int i=0;i<15;i++)
    if(a[i]==1)
      deg_a=i;  
  return deg_a;
  } 
  

    
int* mod(int* a,int* b){
  int c[15],*c1,deg_a,deg_b;
  
  while(deg(a)>=deg(b)){
    for(int i=0;i<15;i++)
      c[i]=0;
    
    c[deg(a)-deg(b)]=1;
    c1=mult(b,&c[0]);
    for(int i=0;i<15;i++)
      a[i]=(a[i]+c1[i])%2;  
    }
  return a;
  }
  
  
//					functions for AES
  
int* field_mult(int* a,int* b){
  int *c,*gen_poly;
  c=mult(a,b);  
  int gen_poly1[15]={1,1,0,1,1,0,0,0,1,0,0,0,0,0,0};
  gen_poly=&gen_poly1[0];
  a=mod(c,gen_poly);
  return a;
  }        
  
   
int* field_inv(int* a){
  int *c,*gen_poly,count;
  c=(int*)malloc(15*sizeof(int));
  int gen_poly1[15]={1,1,0,1,1,0,0,0,1,0,0,0,0,0,0};
  gen_poly=&gen_poly1[0];
  
  for(int i=1;i<256;i++){
    c=binary(i);
    c=mult(c,a);
    c[0]=(c[0]+1)%2;
    c=mod(c,gen_poly);
    count=0;
    for(int j=0;j<15;j++)
      if(c[j]==0)
        count++;
    if(count==15)
      return binary(i);
    }
  }
    

int* subbytes(int* z){
  int count=0,*c,b[15];
  for(int i=0;i<15;i++)
    if(z[i]==0)
      count++;
  if(count!=15)
    z=field_inv(z);  
   
  int c1[15]={1,1,0,0,0,1,1,0,0,0,0,0,0,0,0};
  c=&c1[0];  
    
  for(int i=0;i<8;i++)
    b[i]=(z[(i%8)]+z[(i+4)%8]+z[(i+5)%8]+z[(i+6)%8]+z[(i+7)%8]+c[i])%2;
    
  for(int i=8;i<15;i++)
    b[i]=0;  
  z=&b[0];
  return z;
  }
  
  
int* sub_word(int *a){  
  for(int i=0;i<4;i++)
    a[i]=dec(subbytes(binary(a[i])));
  return a;
  }  
  
  
int* rot_word(int* a){
  int *b;
  b=(int*)malloc(4*sizeof(int));
  
  for(int i=1;i<=4;i++)
    b[i-1]=a[i%4];
  return b;
  }  
    

int** key_expansion(char* key){
  int **rcon,r_con1[10][4],**w,*temp;
  rcon=(int**)malloc(10*sizeof(int*));
  temp=(int*)malloc(4*sizeof(int));
  
  w=(int**)malloc(44*sizeof(int*));
  for(int i=0;i<44;i++)
    w[i]=(int*)malloc(4*sizeof(int));
  
  int rcon_1[10][4]={{0x01,0x00,0x00,0x00},{0x02,0x00,0x00,0x00},{0x04,0x00,0x00,0x00},{0x08,0x00,0x00,0x00},{0x10,0x00,0x00,0x00},{0x20,0x00,0x00,0x00},{0x40,0x00,0x00,0x00},{0x80,0x00,0x00,0x00},{0x1b,0x00,0x00,0x00},{0x36,0x00,0x00,0x00}};
  
  
  for(int i=0;i<10;i++)
    rcon[i]=&rcon_1[i][0];
    
  for(int i=0;i<4;i++)
    for(int j=0;j<4;j++)
      w[i][j]=key[(4*i)+j];  
      
  for(int i=4;i<44;i++){
    for(int j=0;j<4;j++)
      temp[j]=w[i-1][j];
      
    if(i%4==0){
      temp=sub_word(rot_word(temp));  
      for(int j=0;j<4;j++)
        temp[j]=temp[j]^rcon[(i/4)-1][j];
      }
    for(int j=0;j<4;j++)
      w[i][j]=w[i-4][j]^temp[j];
    }
  return w;  
  }         
          
    
int** add_round_key(int** key,int** s){      
   
  for(int i=0;i<4;i++)
    for(int j=0;j<4;j++)
      s[i][j]=key[i][j]^s[i][j];  
      
  return s;     
  }
    
    
int** aes_dec(int** s,int **round_key){
  int **key;
  key=(int**)malloc(4*sizeof(int*));
  for(int i=0;i<4;i++)
    key[i]=(int*)malloc(4*sizeof(int)); 
 
//	last round    
      
  for(int j=0;j<4;j++)
    for(int k=0;k<4;k++)
      key[k][j]=round_key[(4*(9+1))+j][k];  		//transpose
    
  for(int j=0;j<4;j++)
    for(int k=0;k<4;k++)
      s[j][k]=s[j][k]^key[j][k];    
    
  s=inv_shift_rows(s);    

  for(int j=0;j<4;j++)
    for(int k=0;k<4;k++)
      s[j][k]=dec(inv_subbytes(binary(s[j][k])));
 
//	before last round
  
  for(int i=(10-1-1);i>=0;i--){
    for(int j=0;j<4;j++)
      for(int k=0;k<4;k++)
        key[k][j]=round_key[(4*(i+1))+j][k];  		//transpose
    
    for(int j=0;j<4;j++)
      for(int k=0;k<4;k++)
        s[j][k]=s[j][k]^key[j][k];
        
    s=inv_mix_column(s);        
        
    s=inv_shift_rows(s);
        
    for(int j=0;j<4;j++)
      for(int k=0;k<4;k++)
        s[j][k]=dec(inv_subbytes(binary(s[j][k])));  
    }
  
    
//	round 0

  for(int j=0;j<4;j++)
    for(int k=0;k<4;k++)
      key[k][j]=round_key[j][k];  		//transpose 
    
  s=add_round_key(s,key);      
                  
  return s;    
  }  
     
  
//	decrypption main functions    
  
int* matrix_mul(int** a,int* b1){
  int **c,**b,*c1;
  c=(int**)malloc(8*sizeof(int*));
  b=(int**)malloc(8*sizeof(int*));
  c1=(int*)malloc(15*sizeof(int));    
  for(int i=0;i<8;i++){
    c[i]=(int*)malloc(sizeof(int));
    b[i]=(int*)malloc(sizeof(int));
    }
  for(int i=0;i<8;i++)
    b[i][0]=b1[i];
  
  for(int i=0;i<8;i++)
    for(int j=0;j<1;j++){
      c[i][j]=0;
      for(int k=0;k<8;k++)
        c[i][j]=c[i][j]+(a[i][k]*b[k][j]);
      }
  for(int i=0;i<8;i++)
    c1[i]=c[i][0]%2;
  for(int i=8;i<15;i++)
    c1[i]=0;    
  return c1;
  }


int* inv_subbytes(int* b){
  int **a,a1[8][8]={{0,0,1,0,0,1,0,1},{1,0,0,1,0,0,1,0},{0,1,0,0,1,0,0,1},{1,0,1,0,0,1,0,0},{0,1,0,1,0,0,1,0},{0,0,1,0,1,0,0,1},{1,0,0,1,0,1,0,0},{0,1,0,0,1,0,1,0}},constant[15]={1,0,1,0,0,0,0,0,0,0,0,0,0,0,0},x;

  a=(int**)malloc(8*sizeof(int*));
  for(int i=0;i<8;i++)
    a[i]=&a1[i][0];

  b=matrix_mul(a,b);  
  for(int j=0;j<15;j++){
    b[j]=(b[j]+constant[j])%2;
    }
    
  x=dec(b);  

  if(x!=0)
    b=field_inv(binary(x)); 
  return b;
  }


int** inv_mix_column(int** s){
  int **b,**c,b1[4][4]={{14,11,13,9},{9,14,11,13},{13,9,14,11},{11,13,9,14}},p,q,r,s1;
  b=(int**)malloc(4*sizeof(int*));
  for(int i=0;i<4;i++)
    b[i]=&b1[i][0];
  c=(int**)malloc(4*sizeof(int*));    
  for(int i=0;i<4;i++)
    c[i]=(int*)malloc(4*sizeof(int));   
          
  for(int i=0;i<4;i++)
    for(int j=0;j<4;j++){
      for(int k=0;k<4;k++){
        if(k==0)
          p=dec(field_mult(binary(b[i][k]),binary(s[k][j]))); 
        if(k==1)
          q=dec(field_mult(binary(b[i][k]),binary(s[k][j]))); 
        if(k==2)
          r=dec(field_mult(binary(b[i][k]),binary(s[k][j]))); 
        if(k==3)
          s1=dec(field_mult(binary(b[i][k]),binary(s[k][j]))); 
        }
      c[i][j]=p^q^r^s1;  
      }
  return c;
  }


int** inv_shift_rows(int** a){
  int c,d,e,f;
  c=a[1][0];  d=a[1][1];  e=a[1][2];  f=a[1][3];
  a[1][0]=f;  a[1][1]=c;  a[1][2]=d;  a[1][3]=e;
      
  c=a[2][2];  d=a[2][3];
  for(int i=0;i<2;i++)
    a[2][i+2]=a[2][i];
  a[2][0]=c;  
  a[2][1]=d;
  
  c=a[3][0];  d=a[3][1];  e=a[3][2];  f=a[3][3];
  a[3][0]=d;  a[3][1]=e;  a[3][2]=f;  a[3][3]=c;
  return a;
  }


int main(){    
  int *msg_taken,*msg,len_msg=0,len_key=0;
  char y,key[16];
  msg=(int*)malloc(16*sizeof(int));
  msg_taken=(int*)malloc(16*sizeof(int));  
  
//	message entering
  printf("\n** this can decrypt only 16 bit message **\n\n");
  printf("enter the encrypted message (in dec):");
  for(int i=0;i<16;i++)
    scanf("%d",&msg_taken[i]);
    
  getchar();
      
//	key entering  
  printf("enter the key (in text):");
  scanf("%c",&key[0]);  
  while(key[len_key]!='\n'){
    len_key++;
    scanf("%c",&key[len_key]);
    } 
    
  int **round_key,**state;
  printf("\ndecrypted text:");
    
  for(int i=0;i<16;i++)
    msg[i]=msg_taken[i];  

  state=(int**)malloc(4*sizeof(int*));
  for(int i=0;i<4;i++)
    state[i]=(int*)malloc(4*sizeof(int));
    
  for(int i=0;i<16;i++)
    state[i-(4*(i/4))][i/4]=msg[i];    	// formimg state matrix

  round_key=key_expansion(&key[0]);        
  state=aes_dec(state,round_key);
    
  for(int i=0;i<4;i++)
    for(int j=0;j<4;j++)
      printf("%c",state[j][i]); 
	
  printf("\n"); 
  }   
