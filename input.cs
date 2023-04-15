// Este es un comentario de una sola línea 

/*
    Este es un comentario
    con líneas múltiples
*/


using System;

using System;
class Program  
  {  
        public void square(ref int nmbr)  
        {  
            nmbr = nmbr * nmbr;
           // Lets provide a return statement  
           Console.WriteLine("Square of the given number is  " + nmbr);  
         }  
          
        public static void Main(string[] args)  
        {  
           int nmbr = 2; // Value assigned before calling function
           Program pr = new Program(); // Creating a class Object  
          
           Console.WriteLine("The given number is  " + nmbr); //printing the value  
            pr.square( ref  nmbr); //calling by reference using ref keyword
        
        }  
    }

