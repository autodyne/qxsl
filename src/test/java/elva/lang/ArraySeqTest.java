/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import javax.script.ScriptException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link ArraySeq}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class ArraySeqTest extends Assertions {
	private final ListBase list = ArraySeq.from(7, 5, 3);

	@Test
	public void testSize() {
		assertThat(list.size()).isEqualTo(3);
	}

	@Test
	public void testHead() {
		assertThat(list.head()).isEqualTo(new RealNode(7));
	}

	@Test
	public void testTail() {
		assertThat(list.tail()).isEqualTo(ArraySeq.from(5, 3));
	}

	@Test
	public void testInit() {
		assertThat(list.init()).isEqualTo(ArraySeq.from(7, 5));
	}

	@Test
	public void testLast() {
		assertThat(list.last()).isEqualTo(new RealNode(3));
	}

	@Test
	public void testDrop() {
		assertThat(list.drop(0)).isEqualTo(list);
		assertThat(list.drop(1)).isEqualTo(list.tail());
		assertThat(list.drop(2)).isEqualTo(list.tail().tail());
	}

	@Test
	public void testTake() {
		assertThat(list.take(0)).isEqualTo(ArraySeq.from());
		assertThat(list.take(1)).isEqualTo(ArraySeq.from(7));
		assertThat(list.take(2)).isEqualTo(ArraySeq.from(7, 5));
	}

	@Test
	public void testGet() {
		assertThat(list.get(0)).isEqualTo(new RealNode(7));
		assertThat(list.get(2)).isEqualTo(new RealNode(3));
	}

	@Test
	public void testIsEmpty() {
		assertThat(list.isEmpty()).isFalse();
		assertThat(list.take(0).isEmpty()).isTrue();
		assertThat(list.drop(3).isEmpty()).isTrue();
	}

	@Test
	public void testIsAtom() {
		assertThat(list.isAtom()).isFalse();
		assertThat(list.take(0).isAtom()).isTrue();
		assertThat(list.drop(3).isAtom()).isTrue();
	}

	@Test
	public void testValue() {
		assertThat(list.value()).containsExactly(7, 5, 3);
	}

	@Test
	public void testNodes() {
		assertThat(list.nodes()).hasSize(3);
	}

	@Test
	public void testStream() {
		assertThat(list.stream()).hasSize(3);
	}

	@Test
	public void testReals() {
		assertThat(list.reals()).hasSize(3);
	}

	@Test
	public void testToArray() {
		assertThat(list.toArray()).containsExactly(7, 5, 3);
	}

	@Test
	public void testCast() {
		assertThat(list.cast(Integer.class)).containsExactly(7, 5, 3);
	}

	@Test
	public void testContains() {
		assertThat(list.contains(new RealNode(5))).isTrue();
		assertThat(list.contains(new RealNode(2))).isFalse();
	}

	@Test
	public void testEquals() {
		assertThat(list).isEqualTo(ArraySeq.from(7, 5, 3));
	}

	@Test
	public void testToString() {
		assertThat(list).hasToString("(7 5 3)");
	}

	@Test
	public void testIterator() {
		assertThat(list).hasSize(3);
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new ElvaLisp();
		assertThat(elva.eval("'(7 5 3)")).isEqualTo(list.value());
	}
}
