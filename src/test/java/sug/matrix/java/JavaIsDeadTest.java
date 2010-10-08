package sug.matrix.java;
import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;
import static sug.matrix.java.MatrixDouble.*;

import java.util.List;
import java.util.Random;
import java.util.concurrent.Callable;

import org.junit.Test;

import scala.collection.jcl.ArrayList;

public class JavaIsDeadTest {

    public static class Timings {
        public Timings(long l, long m) {
            seq = l;
            par = m;
        }

        public Timings() {
        // TODO Auto-generated constructor stub
        }

        long seq;
        long par;
    }

    @Test
    public void canComputeMatrixMultiplication() throws Exception {
        assertThat(one(2).times(one(2)), is(one(2)));
        MatrixDouble matrix = new MatrixDouble(new double[][] { { 2, 0 }, { 0, 1 } });
        assertThat(one(2).times(one(2)), is(one(2)));
        assertThat(one(2).times(matrix), is(matrix));
        assertThat(matrix.times(matrix), is(new MatrixDouble(new double[][] { { 4, 0 }, { 0, 1 } })));
        MatrixDouble matrix44 = new MatrixDouble(new double[][] { { 2, 0, 1, 1 }, { 0, 1, 2, 2 }, { 0, 1, 0, 0 },
                { 0, 1, 0, 1 } });
        MatrixDouble res = new MatrixDouble(4);
        assertThat(matrix44.power(2, res), is(matrix44.times(matrix44)));
    }

    @Test
    public void canDivideInBlocks() throws Exception {
        MatrixDouble matrix44 = new MatrixDouble(new double[][] { { 2, 0, 1, 1 }, { 0, 1, 2, 2 }, { 0, 1, 0, 0 },
                { 0, 1, 0, 1 } });
        assertThat(matrix44.divideInBlocks(2)[0][0], is(new MatrixDouble(new double[][] { { 2, 0 }, { 0, 1 } })));
        MatrixDouble matrix = new MatrixDouble(new double[][] { { 2, 0 }, { 0, 1 } });
        assertThat(matrix.divideInBlocks(2)[0][0], is(new MatrixDouble(new double[][] { { 2 } })));
    }

    @Test
    public void canMergeMatrices() throws Exception {
        MatrixDouble[][] matrices = new MatrixDouble[][] {
                { new MatrixDouble(new double[][] { { 2, 0 }, { 0, 1 } }),
                        new MatrixDouble(new double[][] { { 1, 1 }, { 2, 2 } }) },
                { new MatrixDouble(new double[][] { { 0, 1 }, { 0, 1 } }),
                        new MatrixDouble(new double[][] { { 0, 0 }, { 0, 1 } }) } };
        MatrixDouble matrix44 = new MatrixDouble(new double[][] { { 2, 0, 1, 1 }, { 0, 1, 2, 2 }, { 0, 1, 0, 0 },
                { 0, 1, 0, 1 } });
        assertThat(merge(matrices), is(matrix44));
        MatrixDouble matrix = new MatrixDouble(new double[][] { { 2, 0 }, { 0, 1 } });
        matrices = new MatrixDouble[][] {
                { new MatrixDouble(new double[][] { { 2 } }), new MatrixDouble(new double[][] { { 0 } }) },
                { new MatrixDouble(new double[][] { { 0 } }), new MatrixDouble(new double[][] { { 1 } }) } };
        assertThat(merge(matrices), is(matrix));
        matrix = new MatrixDouble(new double[][] { { 2, 0, 1 }, { 0, 1, 0 }, { 0, 1, 0 } });
        matrices = new MatrixDouble[][] {
                { new MatrixDouble(new double[][] { { 2 } }), new MatrixDouble(new double[][] { { 0 } }),
                        new MatrixDouble(new double[][] { { 1 } }) },
                { new MatrixDouble(new double[][] { { 0 } }), new MatrixDouble(new double[][] { { 1 } }),
                        new MatrixDouble(new double[][] { { 0 } }) },
                { new MatrixDouble(new double[][] { { 0 } }), new MatrixDouble(new double[][] { { 1 } }),
                        new MatrixDouble(new double[][] { { 0 } }) } };
        assertThat(merge(matrices), is(matrix));
    }

    @Test
    public void canComputeMatrixMultiplicationInParallel() throws Exception {
        MatrixDouble matrix = new MatrixDouble(new double[][] { { 2, 0 }, { 0, 1 } });
        assertThat(one(2).ptimes(matrix), is(matrix));
        MatrixDouble matrix44 = new MatrixDouble(new double[][] { { 2, 0, 1, 1 }, { 0, 1, 2, 2 }, { 0, 1, 0, 0 },
                { 0, 1, 0, 1 } });
        MatrixDouble ptimes = matrix44.ptimes(matrix44);
        assertThat(ptimes, is(matrix44.times(matrix44)));
        assertThat(ptimes, is(matrix44.power(2, new MatrixDouble(4))));
    }

  //    @Test
    public void canCompareTimesBetweenSequentialAndParallelMultiplication() throws Exception {
        for (int i = 128; i < 128 * 10; i += 128) {
            Timings t = compareParAndSeq(i);
            System.err.println(i + " " + t.seq + " " + t.par);
        }
    }

    public static void main(String args[]) throws Exception {
        int len = Integer.parseInt(args[0]);
        ArrayList<Timings> res = new ArrayList<Timings>();
        for (int i = 128; i < 128 * 15; i += 128) {
            Timings t = new Timings();
            for (int j = 0; j < len; j++) {
                Timings t1 = compareParAndSeq(i);
                t.par += t1.par / len;
                t.seq += t1.seq / len;
            }
            System.err.println(i + " " + t.seq + " " + t.par);
        }
    }

    private static Timings compareParAndSeq(int size) throws Exception {
        final MatrixDouble A = generate(size);
        final MatrixDouble B = generate(size);
        return timings(size, new Callable<MatrixDouble>() {

            @Override
            public MatrixDouble call() throws Exception {
                return A.times(B);
            }

        }, new Callable<MatrixDouble>() {

            @Override
            public MatrixDouble call() throws Exception {
                return A.ptimes(B);
            }

        });
    }

    private static <T> Timings timings(int size, Callable<T> seq, Callable<T> par) throws Exception {
        long start = System.nanoTime();
        T res = seq.call();
        long end = (System.nanoTime());
        res = par.call();
        return new Timings((end - start), (System.nanoTime() - end));
    }

    private static MatrixDouble generate(int size) {
        Random rand = new Random();
        double[][] ms = new double[size][size];
        for (int i = 0; i < size; i++)
            for (int j = 0; j < size; j++)
                ms[i][j] = rand.nextInt(5);
        return new MatrixDouble(ms);
    }
}
