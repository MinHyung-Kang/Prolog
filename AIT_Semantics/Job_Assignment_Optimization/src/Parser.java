import java.io.*;
import java.util.Scanner;

/**
 * Created by user on 2015-11-13.
 * Parser used to parse the file into three different files, making it easier for prolog to parse
 */
public class Parser {

    public static final String PEOPLE = "people";
    public static final String OFFERS = "offers";
    public static final String RELATIONSHIPS = "relationships";

    public Parser(){};

    private String people="[", offers="[", relationships="[";
    private int switcher = 0;

    // Read in the file
    protected void read(String name){
        File file  = new File(name);
        try (Scanner fileReader = new Scanner(file)) {
            readLines(fileReader);
            writeFiles(name);
            fileReader.close();
        }catch(FileNotFoundException ex){
            System.out.println("IOException : " + file + " not found.");
            System.out.println(ex);
        }catch(UnsupportedEncodingException ex){
            System.out.println("UnsupportedEncodingException : " + ex);
        }

    }

    //Read in lines, process accordingly
    private void readLines(Scanner scanner) {
        String line;
        while(scanner.hasNextLine()){
            line = scanner.nextLine();

            if(line.equals(PEOPLE)){
                switcher = 1;
                continue;
            }else if(line.equals(OFFERS)){
                switcher = 2;
                continue;
            }else if(line.equals(RELATIONSHIPS)){
                switcher = 3;
                continue;
            }else if(line.equals("")){
                switcher = 0;
            }

            concatLine(line);
        }
    }

    //Concat line to correct string
    private void concatLine(String line) {
        int index,index2;
        String name, name2, type, job, jobType, location, relType;
        switch(switcher){
            case 1:
                index = line.indexOf("|");
                name = line.substring(0,index-1).trim().toLowerCase();
                type = line.substring(index+1,line.length()).trim().replace(" ", "_").toLowerCase();
                people = people.concat("["+name+","+type+"],");
                break;
            case 2:
                index = line.indexOf("|");
                name = line.substring(0,index-1).trim().toLowerCase();
                index2 = line.indexOf("|", index + 1);
                job = line.substring(index+1,index2).trim().replace(" ","_").replace(".","").toLowerCase();
                index = line.indexOf("|", index2 + 1);
                jobType = line.substring(index2+1,index).trim().replace(" ","_").toLowerCase();
                location = line.substring(index+1,line.length()).trim().replace(" ","_").replace(",","").toLowerCase();
                offers = offers.concat("["+name+","+job+","+jobType+","+location+"],");
                break;
            case 3:
                index = line.indexOf("|");
                name = line.substring(0,index-1).trim().toLowerCase();
                index2 = line.indexOf("|", index + 1);
                name2 = line.substring(index+1,index2).trim().replace(" ","_").toLowerCase();
                relType = line.substring(index2+1,line.length()).trim().replace(" ","_").toLowerCase();
                String relationship = getRelationshipPredicate(relType, name, name2);
                relationships = relationships.concat(relationship+",");
                break;
            default:
                break;
        }
    }

    // Convert the string for prolog to understand
    private String getRelationshipPredicate(String relType, String name, String name2) {
        String relationship="";
        switch(relType){
            case "mortal_enemies":
                relationship = "me("+name+","+name2+")";
                break;
            case "dating":
                relationship = "d("+name+","+name2+")";
                break;
            case "friends":
                relationship = "f("+name+","+name2+")";
                break;
            case "married":
                relationship = "m("+name+","+name2+")";
                break;
        }


        return relationship;
    }


    //Write the 3 files
    private void writeFiles(String name) throws FileNotFoundException, UnsupportedEncodingException {

        people = people.substring(0,people.length()-1).concat("].");
        offers = offers.substring(0,offers.length()-1).concat("].");
        relationships = relationships.substring(0,relationships.length()-1).concat("].");

        PrintWriter writer = new PrintWriter(PEOPLE+name, "UTF-8");
        writer.println(people);
        writer.close();
        writer = new PrintWriter(OFFERS+name, "UTF-8");
        writer.println(offers);
        writer.close();
        writer = new PrintWriter(RELATIONSHIPS+name, "UTF-8");
        writer.println(relationships);
        writer.close();
    }

    public static void main(String[] args){
        Parser parser = new Parser();

        Scanner inputScanner = new Scanner(System.in);
        System.out.println("Input file name : ");
        String fileName = inputScanner.nextLine();
        parser.read(fileName);
    }
}
