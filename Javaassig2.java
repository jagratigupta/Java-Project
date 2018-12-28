/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package javaassig2;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.RandomAccessFile;
import java.util.Arrays;


class SampleChange
{
    int rows;
    String s1,s2;
    Double A[][],B[][];
    SampleChange(){};
    SampleChange(String s1,String s2) throws FileNotFoundException, IOException
    {
        this.s1=s1;
        getnumberOfLines(s1);
         A=getMatrix(s1);
         this.s2=s2;
         getnumberOfLines(s2);
         B=getMatrix(s2);
         
    }
    public Double[][] getMatrix(String f1) throws FileNotFoundException, IOException
    {
      RandomAccessFile file = new RandomAccessFile(f1, "r");
      file.readLine();
      int cols=getcols(f1);
      Double[][] matrix=new Double[rows][cols]; 
          for(int i=0;i<rows;i++)
          {
               String l3=file.readLine();
               String l4[]=l3.split(",");
              for(int j=0;j<cols;j++)
              {
              matrix[i][j]=Double.parseDouble(l4[j+1]);
              }
      
          } 
          
      return matrix;
    }
     final void getnumberOfLines(String s1)                       //To get Number of Rows for out Matrix
    {
        int lines=0;
        try
            {
               FileReader input = new FileReader(s1);
               LineNumberReader count = new LineNumberReader(input);
               while (count.skip(Long.MAX_VALUE) > 0)
               {
                  
               }
               lines = count.getLineNumber();
               
            } catch(IOException e)
            {
                System.out.println(e);
            }
        
        rows=lines-1;
    }
     public int getcols(String f1) throws FileNotFoundException, IOException
     {
         RandomAccessFile file = new RandomAccessFile(f1, "r");
      String l1 =file.readLine();
      String l2[]=l1.split(",");
      int cols=l2.length-1;
      return cols;
     }
     Double[] getRowMean(Double M[][]) throws FileNotFoundException, IOException
    {
        Double[] avg=new Double[rows];
          Double sum=0.0;
          for(int i=0;i<rows;i++)
          {
             avg[i]=getRowMean(M[i]);
             //System.out.println(avg[i]);
          }
      
      return avg;
    }
    
     Double getRowMean(Double M[])
    {
        Double sum=0.0;
        for(Double a:M)
        {
            sum=sum+a;
        
        }
        //System.out.println(sum/M.length);
        return sum/M.length;
    }
    //Median
     static Double[] getRowMedian(Double m[][])
    {
        Double median[]=new Double[m.length];
        for(int i=0; i<m.length; i++)
        {
            median[i]=m[i][0];
            Double row[]=m[i];
            Arrays.sort(row);
            median[i]=row[(1+row.length)/2];            
        }
        return median;
    }
    //Get Median
    static Double getRowMedian(int k,Double m[][])
    {
        Double median=0.0;
        median=m[k][0];
        Double row[]=m[k];
        Arrays.sort(row);
        median=row[(1+row.length)/2];            
        return median;
    }
    public Double[] meanFoldChange() throws IOException
    {
       Double fc[]=new Double[rows];
       Double avgn[]=getRowMean(A);
       Double avgt[]=getRowMean(B);
        for(int i=0;i<rows;i++)
        {
            fc[i]=Math.max(avgn[i],avgt[i])/Math.min(avgn[i],avgt[i]);
            
        }
        return fc;
    }
    
    public Double[] variance(Double M[][]) throws FileNotFoundException, IOException
     {
        //Double[][] matrix=getMatrix(s1);
        Double[] avg=getRowMean(M);
        Double[] var=new Double[rows];
        int i=0;
        for(Double x[]:M)
        {
            Double temp=0.0;
            for(Double y:x)
            {
                temp=temp+Math.pow((y-avg[i]),2);
            }
          var[i]=temp/x.length;
          i++;
        }
        return var;
     }
     Double var(int i,Double [][]A) //RowWise
    {
        Double x,XA;
        XA=0.0;
        for(int j=0; j<A[0].length; j++)
        {
            Double t1=A[i][j]-getRowMean(A[i]); //t1=xi-x`
            XA+=(t1*t1);                        //Summation(t1^2)
        }
        Double lenA=new Double(A[0].length);    
        x=(XA)/lenA;                            //Summation(t1^2)/n
        return x;
    }
    public Double[] ttest() throws FileNotFoundException, IOException
    {
        int col1=getcols(s1);
        int col2=getcols(s2);
        Double avg1[]=getRowMean(A);
        Double avg2[]=getRowMean(B);
        Double var1[]=variance(A);
        Double var2[]=variance(B);
        Double[] ttest=new Double[rows];
        for(int i=0;i<rows;i++)
        {
            ttest[i]=Math.abs(avg1[i]-avg2[i])/Math.sqrt((var1[i]/col1)+(var2[i]/col2));
        }
        return ttest;
    }
    
    Double[] shrinkageTtest() throws FileNotFoundException, IOException
    {
        Double tk[]=new Double[rows];
        Double varX[],varY[];
        varX=variance(A);                         
        varY=variance(B);  
        for(int i=0; i<rows; i++)
        {             
            tk[i]=Math.abs(varX[i]-varY[i]);                              //X`k1-X`k2
            Double vk1=VstarK(i,A);                     //V*k1/n1 
            Double vk2=VstarK(i,B);                     //V*k2/n2
            tk[i]=tk[i]/(Math.sqrt(vk1+vk2));           //(X`k1-X`k2)/sqrt(v*k1/n1+v*k2/n2)
        }
        return tk;
    }
    //Wk = (1/n * Summation((xik-x`k)^2))
    
    Double Wk(int k,Double M[][])
    {
        Double XI=0.0;
        Double temp=getRowMean(M[k]);
        for(int i=0; i<M[k].length; i++)
        {
            Double t=(M[k][i]-temp);
            XI+=(t*t);
        }
        return XI/M[k].length;
    }
    //Variance(Vk) = (n/(n-1)^3 * Summation(Wk-W`k)^2)
    Double Variance(int k,Double M[][])
    {
        Double variance=0.0;
        
        for(int i=0; i<M[k].length; i++)
        {
            Double wik=(M[k][i]-Wk(k,M));
            variance+=(wik*wik);
        }
        Double n=Double.valueOf(M[k].length);
        variance = (variance*n)/((n-1)*(n-1)*(n-1));
        return variance;
    }
    Double VstarK(int k, Double M[][]){             //Formula for Vk*/n
        Double vk=0.0;
        Double n=Double.valueOf(M[k].length);
        vk=lambda(k,M)*(getRowMedian(k,M));       //Median For kth Row
        vk+=(1-lambda(k,M))*var(k, M);            //(Y*Vmedian+(1-Y)*Var[k])
        return vk/n;
    }
    Double Vk(int i,Double M[][]){
        Double n=Double.valueOf(M[i].length);
        return (n/(n-1)*Wk(i,M));
    }
    //Summation (Variance(Vk))
    Double lambda(int k, Double M[][])                   //Formula for Lambda*
    {
        Double y=0.0;
        Double numerator=0.0;
        Double denominator=0.0;
        for(int i=0; i<M.length; i++)
        {
            numerator+=Variance(k,M);
            Double x=(Vk(i, M)-getRowMedian(k, M));
            denominator+=(x*x);
        }   
        y=numerator/denominator;
        return (Math.min(1.0, y));                //Min[1,{Summation(Variance(Vk))/Summation((Vk-Vmedian)^2}]
    }
    public void compute(String s1) throws IOException
    {
        File file = new File(s1);
         
        FileWriter fw=new FileWriter(file);
        Double fc[]= shrinkageTtest();
        //Double fc[]= ttest();
        for(int i=0;i<rows;i++)
        {
        String str="g" + (i+1)+"," + fc[i]+"\t";
            System.out.println(str);
            fw.write(str);
             fw.write(System.getProperty( "line.separator" ));
        }
        fw.flush();
      fw.close();
    }
    public void compute(String s1,int x) throws IOException
    {
        File file = new File(s1);
        FileWriter fw=new FileWriter(file);
        Double fc[]= meanFoldChange();
        for(int i=0;i<rows;i++)
        {
            if(fc[i]>=x)
            {
            String str="g" + (i+1)+"," + fc[i]+"\t";
            System.out.println(str);
            fw.write(str);
             fw.write(System.getProperty( "line.separator" ));
            }
        }
        fw.flush();
      fw.close();
    }
}

public class Javaassig2 {

    
    public static void main(String[] args) throws FileNotFoundException, IOException {
      
        
        SampleChange obj=new SampleChange(args[0],args[1]) ;
        obj.compute(args[2]);
        //obj.compute(args[2],2);
    }
    
}
