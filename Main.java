import gherkin.deps.com.google.gson.internal.$Gson$Preconditions;
// used to calc concordance
import java.util.Arrays;
import java.util.Scanner;
// first line of input is the amount of columns of data
// second line contains the number of rows for each column
// then the next lines are just the data, each element is on a new line
public class Main {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int numCols = sc.nextInt();
        int numRows = sc.nextInt();
        int [][] data = new int[numCols+1][numRows+1];
        for (int i = 1; i <= numCols; i++) {
            for (int j = 1; j <= numRows; j++) {
                data[i][j] = sc.nextInt();
            }
        }
        int [] actual = new int[numRows+1];
        for (int i = 1; i <= numRows; i++) actual[i] = sc.nextInt();
        for (int [] arr: data) System.out.println(Arrays.toString(arr));
        // getting concordance value between two columns
        for (int i = 1; i <= numCols-1; i++) {
            for (int j = i+1; j <= numCols; j++) {
                int numSame = 0;
                for (int x = 1; x <= numRows; x++) {
                    if (data[i][x] == data[j][x]) numSame++;
                }
                System.out.println("concordance value between columns " + i + " and " + j + " is " + ((double)numSame/numRows));
            }
        }
        int numMatch = 0;
        int majorityMatch = 0;
        for (int i = 1; i <= numRows; i++) {
            int numZero = 0;
            int numOne = 0;
            for (int j = 1; j <= numCols; j++) {
                if (data[j][i]==0) numZero++;
                else numOne++;
            }
            if (numZero > numOne && actual[i] == 0) {
                majorityMatch++;
            }
            else if (numOne > numZero && actual[i] == 1){
                majorityMatch++;
            }
            if (numZero >= 4 || numOne >= 4) numMatch++;
        }
        System.out.println("percent of at least 4 matches between models for a testing input " + ((double)numMatch/numRows));
        System.out.println("accuracy rate of using the results of the majority of the models for each input " + ((double)majorityMatch)/numRows);
    }
}
