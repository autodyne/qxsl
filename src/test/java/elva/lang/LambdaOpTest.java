/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.LinkedList;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import gaas.utils.AssetUtils;

/**
 * {@link LambdaOp}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/10
 */
public final class LambdaOpTest extends Assertions {
	@ParameterizedTest
	@MethodSource("source")
	public void testGetMaximumArgumentLength(NodeBase node, String sexp) {
		final var val = Integer.valueOf(sexp.split(";")[4]);
		assertThat(node.form().getMaximumArgumentLength()).isEqualTo(val);
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testGetMinimumArgumentLength(NodeBase node, String sexp) {
		final var val = Integer.valueOf(sexp.split(";")[3]);
		assertThat(node.form().getMinimumArgumentLength()).isEqualTo(val);
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testIsAtom(NodeBase node, String sexp) {
		assertThat(node.isAtom()).isTrue();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testIsList(NodeBase node, String sexp) {
		assertThat(node.isList()).isFalse();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testIsName(NodeBase node, String sexp) {
		assertThat(node.isName()).isFalse();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testIsNativeOperator(NodeBase node, String sexp) {
		assertThat(node.form().isNativeOperator()).isFalse();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testIsNull(NodeBase node, String sexp) {
		assertThat(node.isNull()).isFalse();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testIsVarArgs(NodeBase node, String sexp) {
		final var val = Boolean.valueOf(sexp.split(";")[2]);
		assertThat(node.form().isVarArgs()).isEqualTo(val);
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testIterator(NodeBase node, String sexp) {
		assertThat(node).containsExactly(node);
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testToString(NodeBase node, String sexp) {
		assertThat(node).hasToString(sexp.split(";")[1]);
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testValue(NodeBase node, String sexp) {
		assertThat(node.value()).isSameAs(node);
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testValueAsString(NodeBase node, String sexp) {
		assertThat(node.valueAsString()).hasToString(sexp.split(";")[1]);
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testWrap(NodeBase node, String sexp) {
		assertThat(NodeBase.wrap(node)).isSameAs(node);
	}

	private static final List<Arguments> source() throws Exception {
		final var elva = new ElvaLisp();
		final var list = new LinkedList<Arguments>();
		final var util = new AssetUtils(LambdaOp.class);
		for (var line : util.listLines("LambdaOp.lisp")) {
			list.add(Arguments.of(elva.eval(line), line));
		}
		return list;
	}
}
