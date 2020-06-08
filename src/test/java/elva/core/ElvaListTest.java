/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.util.Arrays;
import javax.script.ScriptException;
import org.junit.jupiter.api.Test;

import static elva.core.ElvaList.array;
import static elva.core.ElvaList.chain;

/**
 * {@link ElvaList}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class ElvaListTest extends org.assertj.core.api.Assertions {
	private final ElvaList array = array(Arrays.asList(7, 5, 3));
	private final ElvaList chain = chain(Arrays.asList(3, 6, 9));

	@Test
	public void testHead() {
		assertThat(array.head()).isEqualTo(new ElvaReal(7));
		assertThat(chain.head()).isEqualTo(new ElvaReal(3));
	}

	@Test
	public void testTail() {
		assertThat(array.tail()).isEqualTo(chain(Arrays.asList(5, 3)));
		assertThat(chain.tail()).isEqualTo(array(Arrays.asList(6, 9)));
	}

	@Test
	public void testDrop() {
		assertThat(array.drop(0)).isEqualTo(array);
		assertThat(chain.drop(0)).isEqualTo(chain);
		assertThat(array.drop(1)).isEqualTo(array.tail());
		assertThat(chain.drop(1)).isEqualTo(chain.tail());
		assertThat(array.drop(2)).isEqualTo(array.tail().tail());
		assertThat(chain.drop(2)).isEqualTo(chain.tail().tail());
	}

	@Test
	public void testTake() {
		assertThat(array.take(0)).isEqualTo(chain(Arrays.asList()));
		assertThat(chain.take(0)).isEqualTo(array(Arrays.asList()));
		assertThat(array.take(1)).isEqualTo(chain(Arrays.asList(7)));
		assertThat(chain.take(1)).isEqualTo(array(Arrays.asList(3)));
		assertThat(array.take(2)).isEqualTo(chain(Arrays.asList(7, 5)));
		assertThat(chain.take(2)).isEqualTo(array(Arrays.asList(3, 6)));
	}

	@Test
	public void testGet() {
		assertThat(array.get(0)).isEqualTo(new ElvaReal(7));
		assertThat(chain.get(0)).isEqualTo(new ElvaReal(3));
		assertThat(array.get(2)).isEqualTo(new ElvaReal(3));
		assertThat(chain.get(2)).isEqualTo(new ElvaReal(9));
	}

	@Test
	public void testSize() {
		assertThat(array.size()).isEqualTo(3);
		assertThat(chain.size()).isEqualTo(3);
	}

	@Test
	public void testIsEmpty() {
		assertThat(array.isEmpty()).isFalse();
		assertThat(chain.isEmpty()).isFalse();
		assertThat(array.take(0).isEmpty()).isTrue();
		assertThat(chain.take(0).isEmpty()).isTrue();
		assertThat(array.drop(3).isEmpty()).isTrue();
		assertThat(chain.drop(3).isEmpty()).isTrue();
	}

	@Test
	public void testLast() {
		assertThat(array.last()).isEqualTo(new ElvaReal(3));
		assertThat(chain.last()).isEqualTo(new ElvaReal(9));
	}

	@Test
	public void testEquals() {
		assertThat(array).isEqualTo(array(Arrays.asList(7, 5, 3)));
		assertThat(chain).isEqualTo(chain(Arrays.asList(3, 6, 9)));
	}

	@Test
	public void testContains() {
		assertThat(array.contains(new ElvaReal(5))).isTrue();
		assertThat(chain.contains(new ElvaReal(9))).isTrue();
		assertThat(array.contains(new ElvaReal(2))).isFalse();
		assertThat(chain.contains(new ElvaReal(8))).isFalse();
	}

	@Test
	public void testReals() {
		assertThat(array.reals()).hasSize(3);
		assertThat(chain.reals()).hasSize(3);
	}

	@Test
	public void testValue() {
		assertThat(Arrays.asList(array.value())).containsExactly(7, 5, 3);
		assertThat(Arrays.asList(chain.value())).containsExactly(3, 6, 9);
	}

	@Test
	public void testToArray() {
		assertThat(array.toArray(Integer.class)).containsExactly(7, 5, 3);
		assertThat(chain.toArray(Integer.class)).containsExactly(3, 6, 9);
	}

	@Test
	public void testStream() {
		assertThat(array.stream()).hasSize(3);
		assertThat(chain.stream()).hasSize(3);
	}

	@Test
	public void testToString() {
		assertThat(array).hasToString("(7 5 3)");
		assertThat(chain).hasToString("(3 6 9)");
	}

	@Test
	public void testIterator() {
		assertThat(array).hasSize(3);
		assertThat(chain).hasSize(3);
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new elva.lang.ElvaRuntime();
		assertThat(elva.eval("'(7 5 3)")).isEqualTo(array.value());
		assertThat(elva.eval("'(3 6 9)")).isEqualTo(chain.value());
	}
}
