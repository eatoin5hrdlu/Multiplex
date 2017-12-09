entity fulladder is
port (signal ina,inb, cin: in bit;
      signal cout,sum: out bit);
end fulladder;


architecture behavior of fulladder is
signal s1,s2,s3: bit;
begin
  s1 <= ina xor inb;
  sum <= cin xor s1;
 s2 <= cin and s1; 
 s3 <= ina and inb;
 cout <= s2 or s3;
end behavior;
