package org.lolhens.inject.test.testclasses;

/**
 * Created by LolHens on 17.12.2014.
 */
public class Testclass1 {
    private int test = 0;
    private static int test2 = 0;

    public int test1(int i1) {
        int a = 2;
        test = 1;
        test2 = 1;
        for (int i = 0; i < 3; i++) {
            test2(i);
        }
        System.out.println("test");
        switch (3) {
            case 1:
                break;
            case 10000:
                break;
        }
        switch (3) {
            case 0:
                break;
            case 1:
                break;
            case 2:
                break;
            case 3:
                break;
        }
        return 0;
    }

    private void test2(int i) {

    }
}
