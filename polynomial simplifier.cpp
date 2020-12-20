#include <iostream>
#include <vector>
#include <string>
#include <locale>

using namespace std;

//holds 1 term of a polynomial
struct term {
    double coeficient;
    vector<double> powers;
    vector<char> vars;
};

//each polynomial is just a list of pointers to terms.
struct poly {
    vector<struct term*> temrs;
};

//global variable which holds pointers to all polynomials in order of creation
vector<struct poly*> polys;

string makeLower(string str) {
    string newStr = "";
    string tempStr = " ";
    locale temp;
    for (string::size_type i = 0; i < str.length(); ++i) {
        tempStr = " ";
        tempStr[0] = tolower(str[i], temp);
        newStr = newStr.append(tempStr);
    }
    return newStr;
}

//converts string to num while checking for invalid string. returns -1 for invalid.
double strToNum(string str) {
    double num;

    try {
        num = stod(str);
    }
    catch (...) {
        num = -1;
    }
    return num;
}

//returns the first index of a lowercase letter. returns -1 if there is none.
int getIndexFirstLetter(string str) {

    int index = INT_MAX;
    int temp;

    for (char i = 'a'; i <= 'z'; i++) {
        temp = str.find(i);
        if (temp != string::npos && temp < index) {
            index = temp;
        }
    }

    if (index == INT_MAX) {
        index = -1;
    }

    return index;
}

void printPolys() {
    
    


    bool isZero;

    cout << endl << "=======================" << endl;
    for (int i = 0; i < polys.size(); i++) {
        isZero = true;
        for (int j = 0; j < polys[i]->temrs.size(); j++) {

            if (polys[i]->temrs[j]->coeficient != 0) {
                isZero = false;
                if (polys[i]->temrs[j]->coeficient != 1 || polys[i]->temrs[j]->vars.empty()) {
                    cout << polys[i]->temrs[j]->coeficient;
                }

                for (int k = 0; k < polys[i]->temrs[j]->vars.size(); k++) {
                    cout << polys[i]->temrs[j]->vars[k];
                    if (polys[i]->temrs[j]->powers[k] != 1) {
                        cout << "^" << polys[i]->temrs[j]->powers[k];
                    }
                }
                if (j + 1 != polys[i]->temrs.size()) {
                    cout << "+";
                }
            }
            
        }
        if (isZero) {
            cout << 0;
        }
        cout << endl;
    }
    
    if (polys.empty()) {
        cout << "MEMORY EMPTY" << endl;
    }
    cout << "=======================" << endl;
}


//parses a term into a struct and returns a pointer to the struct. if the term is invalid it returns NULL;
struct term * parseTerm(string strTerm, bool isPositive) {

    //creates a new term and sets the coeficient to 1 by default
    struct term* newTerm = new struct term;
    newTerm->coeficient = 1;

    //initialize local variables
    bool valid = true;
    bool negPower;
    string strCoeficient;
    int endPower;
    int repeatIndex;
    double pow;

    //get index of the first letter to determine if there is a leading coeficient or not
    int firstLetter = getIndexFirstLetter(strTerm);

    switch (firstLetter) {
    case -1:
        //has coeficient with no variables
        newTerm->coeficient = strToNum(strTerm);
        if (newTerm->coeficient == -1) {
            valid = false;
        }
        break;

    default:
        //has both
        strCoeficient = strTerm;
        strTerm.erase(0, firstLetter);
        strCoeficient.erase(firstLetter);

        newTerm->coeficient = strToNum(strCoeficient);

        if (newTerm->coeficient == -1) {
            valid = false;
            break;
        }

    case 0:
        //no coeficient

        //loop through variables checking for a power for each
        while (!strTerm.empty() && valid) {
            negPower = false;
            repeatIndex = -1;
            pow = 1;

            for (int i = 0; i < newTerm->vars.size(); i++) {
                if (newTerm->vars[i] == strTerm[0]) {
                    repeatIndex = i;
                    break;
                }
            }

            if (repeatIndex == -1) {
                newTerm->vars.push_back(strTerm[0]);
            }
            
            strTerm.erase(0,1);

            //parses the power if there is one
            if (!strTerm.empty() && strTerm[0] == '^') {
                
                strTerm.erase(0, 1);
                if (strTerm[0] == '-') {
                    negPower = true;
                    strTerm.erase(0, 1);
                }

                endPower = getIndexFirstLetter(strTerm);

                if (endPower == -1) {

                    pow = strToNum(strTerm);

                    strTerm = "";
                }
                else {

                    strCoeficient = strTerm;
                    strCoeficient.erase(endPower);
                    pow = strToNum(strCoeficient);

                    strTerm.erase(0,endPower);
                }
                

            }

            if (pow == -1) {
                valid = false;
            }
            
            if (negPower) {
                pow = pow * -1;
            }

            if (repeatIndex == -1) {
                newTerm->powers.push_back(pow);
            }
            else {
                newTerm->powers[repeatIndex] = newTerm->powers[repeatIndex] + pow;
            }

            
            
        }


        break;
    }

    if (!valid) {
        newTerm = NULL;
    }
    else {
        //sort term
        for (int i = 0; i < newTerm->vars.size(); i++) {
            for (int j = i + 1; j < newTerm->vars.size(); j++) {
                if (newTerm->vars[i] > newTerm->vars[j]) {
                    char temp = newTerm->vars[i];
                    newTerm->vars[i] = newTerm->vars[j];
                    newTerm->vars[j] = temp;
                    double tempNum = newTerm->powers[i];
                    newTerm->powers[i] = newTerm->powers[j];
                    newTerm->powers[j] = tempNum;

                }
            }
        }
    }

    if (newTerm != NULL && !isPositive) {
        newTerm->coeficient = newTerm->coeficient * -1;
    }

    return newTerm;
}


//parses the polynomial from the string into the poly structure
//the string polynomial can only have numbers, letters, and the following special characters (NOTE: NO SPACES ALLOWED)
// '+'  '-'  '.'  '^'
//case is irelivant to the parsing and will all be made lower case.
//example
//4x^2+5.2X-x^3y^2+2
bool parsePoly(string strPoly) {

    //local variables
    bool termSuccessfull = true;
    bool nxtPositive;
    bool isPositive = true;
    bool valid = true;
    int firstPlus;
    int firstMinus;
    int nxtSign;
    int repeatIndex;
    string nxtTerm;
    struct term* newTerm;

    if (!strPoly.empty()) {
        //create a new poly structure
        struct poly* newPoly = new struct poly;
        
        

        //format the string
        strPoly = makeLower(strPoly);

        while (!strPoly.empty() && valid) {
            repeatIndex = -1;
            
            if (strPoly[0] == '-' && isPositive) {
                isPositive = false;
                strPoly.erase(0,1);
            }
            else if (strPoly[0] == '-' && !isPositive) {
                isPositive = true;
                strPoly.erase(0, 1);
            }

            //isolate and remove first term
            firstPlus = strPoly.find('+');
            firstMinus = strPoly.find('-');

            //sets up nxtTerm string
            if (firstMinus == string::npos && firstPlus == string::npos) {
                //when there is not a plus or minus we are on the last term
                nxtTerm = strPoly;
                strPoly = "";
                nxtPositive = true;
            }
            else {
                //when there is a plus or minus determine which exists and which is closer and set nxtSign to that signs index
                if (firstMinus == string::npos) {
                    nxtSign = firstPlus;
                    nxtPositive = true;
                }
                else if (firstPlus == string::npos) {
                    nxtSign = firstMinus;
                    nxtPositive = false;
                }
                else if (firstPlus < firstMinus) {
                    nxtSign = firstPlus;
                    nxtPositive = true;
                }
                else {
                    nxtSign = firstMinus;
                    nxtPositive = false;
                }

                //create the string for the next term
                nxtTerm = strPoly;
                nxtTerm.erase(nxtSign);
                //iterates strPoly towared exit condition by removing the term
                strPoly.erase(0, nxtSign + 1);
                
            }

            //parse the nxtTerm and determine if valid
            newTerm = parseTerm(nxtTerm, isPositive);
            if (newTerm == NULL) {
                valid = false;
            }
            else {
                valid = true;
                //simplify
                for (int i = 0; i < newPoly->temrs.size(); i++) {
                    struct term* tempTerm = newPoly->temrs[i];
                    if (tempTerm->vars.size() == 0 && newTerm->vars.size() == 0) {
                        repeatIndex = i;
                    }
                    else if (tempTerm->vars.size() == newTerm->vars.size()) {
                        for (int j = 0; j < tempTerm->vars.size(); j++) {
                            if (tempTerm->vars[j] != newTerm->vars[j] || tempTerm->powers[j] != newTerm->powers[j]) {
                                break;
                            }
                            if (j + 1 == tempTerm->vars.size()) {
                                repeatIndex = i;
                            }
                        }
                    }
                    if (repeatIndex == i) {
                        break;
                    }
                }

                if (repeatIndex == -1) {
                    newPoly->temrs.push_back(newTerm);
                }
                else {
                    newPoly->temrs[repeatIndex]->coeficient = newPoly->temrs[repeatIndex]->coeficient + newTerm->coeficient;
                }

                
            }
            
            isPositive = nxtPositive;

        }
        if (valid) {
            //add it to the global list if valid
            polys.push_back(newPoly);
        }

    }
    
    

    return valid;
}


//menu function
void printMenu() {
    cout << endl << "=======================" << endl;
    cout << "Please enter the letter of the option you want to choose:" << endl;
    cout << "I: Input polynomial" << endl;
    cout << "P: Print all saved polynomials" << endl;
    cout << "C: Clear all saved polynomials" << endl;
    cout << "Q: Quit" << endl;
    cout << "=======================" << endl << endl;
}

int main() {

    string temp;
    string temp2;
    int tempInt;


    //initialize local variables
    char menuChoice = 0;
    string userInput, strPoly;

    //loop untill user chooses to exit
    while (menuChoice != 'q' && menuChoice != 'Q') {
        //set loop values to defaults to prevent errors
        userInput = "";
        strPoly = "";
        menuChoice = 0;
        
        //print menu and get choice from the user
        printMenu();
        cin >> userInput;

        //check the user input something and only take the first character
        if (userInput.empty()) {
            menuChoice = 0;
        }
        else {
            menuChoice = userInput[0];
        }

        //switch on userInput to determine what is done checking both cases of each entry letter
        switch (menuChoice) {

        case 'I':
        case 'i':
            //Input polynomial

            //get poly from user as a string
            cout << "Enter a polynomial:" << endl;
            cin >> strPoly;
            
            //hands the polynomial to the parse function
            parsePoly(strPoly);

            break;

        case 'P':
        case 'p':
            //Print all saved polynomials
            printPolys();

            break;

        case 'C':
        case 'c':
            //Clear all saved polynomials
            polys.clear();
            cout << "MEMORY CLEARED" << endl;

            break;

        case 'Q':
        case 'q':
            //Quit

            //checks if the user wants to quit and warns data will not be saved
            cout << "Are you sure you want to exit?" << endl; 
            cout << "WARNING! YOUR POLYNOMIALS WILL NOT BE SAVED" << endl;
            cout << "enter 'Y' for yes and anything else for no." << endl;
            cin >> userInput;

            //if user does not want to quit the menu choice is changed to not be Q so the loop will not end.
            if (userInput.empty() || (userInput[0] != 'y' && userInput[0] != 'Y')) {
                menuChoice = 0;
            }
            else {
                cout << "Exiting" << endl;
            }

            break;

        default:
            //catches faulty entries and alerts the user to them
            cout << "=======================" << endl;
            cout << "ERROR! INVALID ENTRY" << endl;
            cout << "Please enter a single letter only based on the menu option you would like to choose." << endl;
            cout << "=======================" << endl;
            break;
        }


    }

}

