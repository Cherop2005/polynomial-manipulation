#include <iostream>
#include <cmath>
#include <sstream>
#include <string>
#include <regex>

using namespace std;

struct Term {
    double coeff;
    int exp;
    Term* next;

    Term(double c, int e) : coeff(c), exp(e), next(nullptr) {}
};

class Polynomial {
private:
    Term* head;

    void insertSorted(double coeff, int exp) {
        if (abs(coeff) < 1e-9) return;
        Term* newTerm = new Term(coeff, exp);
        if (!head || exp > head->exp) {
            newTerm->next = head;
            head = newTerm;
        } else {
            Term* current = head, *prev = nullptr;
            while (current && current->exp > exp) {
                prev = current;
                current = current->next;
            }
            if (current && current->exp == exp) {
                current->coeff += coeff;
                delete newTerm;
                if (abs(current->coeff) < 1e-9) {
                    if (prev) prev->next = current->next;
                    else head = current->next;
                    delete current;
                }
            } else {
                newTerm->next = current;
                if (prev) prev->next = newTerm;
                else head = newTerm;
            }
        }
    }

public:
    Polynomial() : head(nullptr) {}

    void addTerm(double coeff, int exp) {
        insertSorted(coeff, exp);
    }

    void parseFromString(const string& expression) {
        string cleaned = regex_replace(expression, regex("\\s+"), ""); // Remove spaces
        regex termPattern("([+-]?\\d*\\.?\\d*)(x(?:\\^\\d+)?)?"); // Corrected regex
        auto termsBegin = sregex_iterator(cleaned.begin(), cleaned.end(), termPattern);
        auto termsEnd = sregex_iterator();

        for (auto i = termsBegin; i != termsEnd; ++i) {
            smatch match = *i;

            if (match.length() == 0) continue;

            string coeffStr = match[1].str();
            string xPart = match[2].str();

            if (coeffStr.empty() && xPart.empty()) continue;

            double coeff = 1.0;
            int exp = 0;

            // Coefficient extraction
            if (!coeffStr.empty()) {
                if (coeffStr == "+" || coeffStr == "-")
                    coeff = (coeffStr == "-") ? -1 : 1;
                else
                    coeff = stod(coeffStr);
            } else if (!xPart.empty()) {
                coeff = 1.0;  // For terms like "+x" or "-x"
            }

            // Exponent extraction
            if (!xPart.empty()) {
                if (xPart == "x")
                    exp = 1;
                else
                    exp = stoi(xPart.substr(2)); // Skips "x^"
            }

            addTerm(coeff, exp);
        }
    }

    void display() const {
        Term* current = head;
        if (!current) { cout << "0\n"; return; }
        while (current) {
            if (current != head && current->coeff > 0) cout << "+";
            if (current->exp == 0) cout << current->coeff;
            else if (current->exp == 1) cout << current->coeff << "x";
            else cout << current->coeff << "x^" << current->exp;
            current = current->next;
        }
        cout << endl;
    }

    Polynomial add(const Polynomial& other) const {
        Polynomial result;
        Term* a = head;
        Term* b = other.head;
        while (a || b) {
            if (!b || (a && a->exp > b->exp)) {
                result.addTerm(a->coeff, a->exp);
                a = a->next;
            } else if (!a || (b && b->exp > a->exp)) {
                result.addTerm(b->coeff, b->exp);
                b = b->next;
            } else {
                result.addTerm(a->coeff + b->coeff, a->exp);
                a = a->next;
                b = b->next;
            }
        }
        return result;
    }

    Polynomial subtract(const Polynomial& other) const {
        Polynomial result;
        Term* a = head;
        Term* b = other.head;
        while (a || b) {
            if (!b || (a && a->exp > b->exp)) {
                result.addTerm(a->coeff, a->exp);
                a = a->next;
            } else if (!a || (b && b->exp > a->exp)) {
                result.addTerm(-b->coeff, b->exp);
                b = b->next;
            } else {
                result.addTerm(a->coeff - b->coeff, a->exp);
                a = a->next;
                b = b->next;
            }
        }
        return result;
    }

    Polynomial multiply(const Polynomial& other) const {
        Polynomial result;
        for (Term* a = head; a; a = a->next) {
            for (Term* b = other.head; b; b = b->next) {
                result.addTerm(a->coeff * b->coeff, a->exp + b->exp);
            }
        }
        return result;
    }

    Polynomial derivative() const {
        Polynomial result;
        for (Term* current = head; current; current = current->next) {
            if (current->exp != 0) {
                result.addTerm(current->coeff * current->exp, current->exp - 1);
            }
        }
        return result;
    }

    Polynomial integrate() const {
        Polynomial result;
        for (Term* current = head; current; current = current->next) {
            result.addTerm(current->coeff / (current->exp + 1), current->exp + 1);
        }
        return result;
    }

    pair<Polynomial, Polynomial> divide(const Polynomial& divisor) const {
        Polynomial quotient, remainder;
        for (Term* temp = head; temp; temp = temp->next)
            remainder.addTerm(temp->coeff, temp->exp);

        while (remainder.head && divisor.head && remainder.head->exp >= divisor.head->exp) {
            double leadCoeff = remainder.head->coeff / divisor.head->coeff;
            int leadExp = remainder.head->exp - divisor.head->exp;
            Polynomial term;
            term.addTerm(leadCoeff, leadExp);
            quotient = quotient.add(term);
            remainder = remainder.subtract(term.multiply(divisor));
        }

        return {quotient, remainder};
    }

    double evaluate(double x) const {
        double result = 0;
        for (Term* current = head; current; current = current->next) {
            result += current->coeff * pow(x, current->exp);
        }
        return result;
    }

    ~Polynomial() {
        while (head) {
            Term* temp = head;
            head = head->next;
            delete temp;
        }
    }
};

int main() {
    Polynomial p1, p2;

    p1.parseFromString("3x^2 + 2x");
    p2.parseFromString("4x + 1");

    cout << "P1: "; p1.display();
    cout << "P2: "; p2.display();

    Polynomial sum = p1.add(p2);
    cout << "Sum: "; sum.display();

    Polynomial diff = p1.subtract(p2);
    cout << "P1 - P2: "; diff.display();

    Polynomial prod = p1.multiply(p2);
    cout << "Product: "; prod.display();

    Polynomial deriv = p1.derivative();
    cout << "Derivative of P1: "; deriv.display();

    Polynomial deriv2 = p2.derivative();
    cout << "Derivative of P2: "; deriv2.display();

    Polynomial integ = p1.integrate();
    cout << "Integral of P1: "; integ.display();

    Polynomial integ2 = p2.integrate();
    cout << "Integral of P2: "; integ2.display();

    auto result = p1.divide(p2);
    Polynomial quotient = result.first;
    Polynomial remainder = result.second;

    cout << "Quotient: "; quotient.display();
    cout << "Remainder: "; remainder.display();

    double val = 2;
    cout << "P1 evaluated at x=" << val << ": " << p1.evaluate(val) << endl;
    cout << "P2 evaluated at x=" << val << ": " << p2.evaluate(val) << endl;

    return 0;
}