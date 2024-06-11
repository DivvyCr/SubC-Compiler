bool palindrome(int number) {
   int t;
   int rev;
   bool result;
   int rmndr;

   t = number;
   rev = 0;   
   result = false;
 
   while (number > 0) {
      rmndr = number%10;
      rev = rev*10 + rmndr;
      number = number/10;
   }

   return (t == rev);
}
