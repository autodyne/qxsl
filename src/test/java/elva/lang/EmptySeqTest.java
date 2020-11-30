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
 * {@link EmptySeq}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/10
 */
public final class EmptySeqTest extends Assertions {
	private final ListBase list = EmptySeq.NIL;

	@Test
	public void testCast() {
		assertThat(list.cast(Integer.class)).containsExactly();
	}

	@Test
	public void testContains() {
		assertThat(list.contains(new RealNode(5))).isFalse();
		assertThat(list.contains(new RealNode(2))).isFalse();
	}

	@Test
	public void testDrop() {
		assertThat(list.drop(0)).isEqualTo(list);
		assertThat(list.drop(1)).isEqualTo(list);
		assertThat(list.drop(2)).isEqualTo(list);
	}

	@Test
	public void testEquals() {
		assertThat(list).isEqualTo(ArraySeq.from());
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new ElvaLisp();
		assertThat(elva.eval("'()")).isEqualTo(list.value());
	}

	@Test
	public void testGet() {
		assertThatThrownBy(() -> list.get(0)).isNotNull();
		assertThatThrownBy(() -> list.get(2)).isNotNull();
	}

	@Test
	public void testHead() {
		assertThat(list.head()).isEqualTo(list);
	}

	@Test
	public void testInit() {
		assertThat(list.init()).isEqualTo(list);
	}

	@Test
	public void testIsAtom() {
		assertThat(list.isAtom()).isTrue();
	}

	@Test
	public void testIsEmpty() {
		assertThat(list.isEmpty()).isTrue();
	}

	@Test
	public void testIterator() {
		assertThat(list).hasSize(0);
	}

	@Test
	public void testLast() {
		assertThat(list.last()).isEqualTo(list);
	}

	@Test
	public void testSize() {
		assertThat(list.size()).isEqualTo(0);
	}

	@Test
	public void testStream() {
		assertThat(list.stream()).hasSize(0);
	}

	@Test
	public void testTail() {
		assertThat(list.tail()).isEqualTo(list);
	}

	@Test
	public void testTake() {
		assertThat(list.take(0)).isEqualTo(list);
		assertThat(list.take(1)).isEqualTo(list);
		assertThat(list.take(2)).isEqualTo(list);
	}

	@Test
	public void testToArray() {
		assertThat(list.toArray()).containsExactly();
	}

	@Test
	public void testToString() {
		assertThat(list).hasToString("()");
	}

	@Test
	public void testValue() {
		assertThat(list.value()).containsExactly();
	}
}
