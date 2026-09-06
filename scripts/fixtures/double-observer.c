#include <math.h>
#include <stdint.h>
#include <stdio.h>

extern double dadd(double, double);
extern double dsub(double, double);
extern double dmul(double, double);
extern double ddiv(double, double);
extern double dneg(double);
extern double dcube(double);
extern double dchain(double, double);
extern int64_t deq(double, double);
extern int64_t dneq(double, double);
extern int64_t dlt(double, double);
extern int64_t dlte(double, double);
extern int64_t dgt(double, double);
extern int64_t dgte(double, double);

static int failures;

static void check_number(const char *name, double actual, double expected) {
    int matches = isnan(expected) ? isnan(actual)
        : actual == expected && !!signbit(actual) == !!signbit(expected);
    if (!matches) {
        fprintf(stderr, "%s: expected %.17g (sign %d), got %.17g (sign %d)\n",
                name, expected, !!signbit(expected), actual, !!signbit(actual));
        ++failures;
    }
}

static void check_predicate(const char *name, double x, double y, int64_t actual, int expected) {
    if (actual != expected) {
        fprintf(stderr, "%s(%.17g, %.17g): expected %d, got %lld\n",
                name, x, y, expected, (long long)actual);
        ++failures;
    }
}

int main(void) {
    /* All finite expected values below are exactly representable in binary. */
    check_number("add", dadd(8.0, 2.0), 10.0);
    check_number("sub", dsub(8.0, 2.0), 6.0);
    check_number("mul", dmul(8.0, 2.0), 16.0);
    check_number("div", ddiv(8.0, 2.0), 4.0);
    check_number("neg", dneg(8.0), -8.0);
    check_number("cube", dcube(5.0), 125.0);
    check_number("chain", dchain(8.0, 2.0), 30.0);
    check_number("fractional add", dadd(-3.5, 0.5), -3.0);
    check_number("fractional sub", dsub(-3.5, 0.5), -4.0);
    check_number("fractional mul", dmul(-3.5, 0.5), -1.75);
    check_number("fractional div", ddiv(-3.5, 0.5), -7.0);
    check_number("fractional neg", dneg(-3.5), 3.5);
    check_number("fractional cube", dcube(-3.5), -42.875);
    check_number("fractional chain", dchain(-3.5, 0.5), 24.0);

    check_number("negative zero add", dadd(-0.0, -0.0), -0.0);
    check_number("negative zero sub", dsub(-0.0, 0.0), -0.0);
    check_number("negative zero mul", dmul(-2.0, 0.0), -0.0);
    check_number("negative zero div", ddiv(0.0, -2.0), -0.0);
    check_number("negate positive zero", dneg(0.0), -0.0);
    check_number("negate negative zero", dneg(-0.0), 0.0);

    check_number("infinity add", dadd(INFINITY, 1.0), INFINITY);
    check_number("infinity sub", dsub(-INFINITY, 1.0), -INFINITY);
    check_number("infinity mul", dmul(-INFINITY, 2.0), -INFINITY);
    check_number("positive infinity div", ddiv(1.0, 0.0), INFINITY);
    check_number("negative infinity div", ddiv(1.0, -0.0), -INFINITY);
    check_number("zero divided by zero", ddiv(0.0, 0.0), NAN);
    check_number("infinity minus infinity", dsub(INFINITY, INFINITY), NAN);
    check_number("infinity times zero", dmul(INFINITY, 0.0), NAN);
    check_number("NaN add", dadd(NAN, 1.0), NAN);
    check_number("NaN neg", dneg(NAN), NAN);

    const double pairs[][2] = {
        {1.0, 2.0}, {2.0, 1.0}, {0.0, 0.0}, {-0.0, 0.0}, {-8.0, -8.0},
        {INFINITY, INFINITY}, {-INFINITY, INFINITY},
        {NAN, 1.0}, {1.0, NAN}, {NAN, NAN},
    };
    for (size_t i = 0; i < sizeof(pairs) / sizeof(pairs[0]); ++i) {
        double x = pairs[i][0], y = pairs[i][1];
        /* Preserve Donato's ordered predicates, including ordered !=. */
        int ordered = !isnan(x) && !isnan(y);
        check_predicate("eq", x, y, deq(x, y), ordered && x == y);
        check_predicate("neq", x, y, dneq(x, y), ordered && x != y);
        check_predicate("lt", x, y, dlt(x, y), ordered && x < y);
        check_predicate("lte", x, y, dlte(x, y), ordered && x <= y);
        check_predicate("gt", x, y, dgt(x, y), ordered && x > y);
        check_predicate("gte", x, y, dgte(x, y), ordered && x >= y);
    }
    if (failures) {
        fprintf(stderr, "%d double checks failed\n", failures);
        return 1;
    }
    puts("PASS double observer");
    return 0;
}
