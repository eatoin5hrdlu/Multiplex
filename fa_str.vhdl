entity fulladder is
port (signal ina,inb, cin: in bit;
      signal cout,sum: out bit);
end fulladder;

architecture structure of fulladder is
signal s1,s2,s3:bit;
component and_gate port (a,b: in bit; c: out bit); end component;
component xor_gate port (a,b: in bit; c: out bit); end component;
component or_gate  port (a,b: in bit; c: out bit);  end component;
begin
 x1: xor_gate port map (ina,inb,s1);
 x2: xor_gate port map (s1,cin,sum);
 a1: and_gate port map (cin,s1,s2);
 a2: and_gate  port map (ina,inb,s3);
 or1: or_gate port map (s2,s3,cout);
end structure;
