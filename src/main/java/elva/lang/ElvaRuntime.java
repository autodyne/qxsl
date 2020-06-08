/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.stream.Collectors;
import javax.script.*;

import elva.bind.*;
import elva.core.*;
import elva.warn.*;

import static javax.script.ScriptContext.ENGINE_SCOPE;
import static javax.script.ScriptContext.GLOBAL_SCOPE;

/**
 * 無線部開発班が実装するLISP処理系の{@link ScriptEngine}の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/17
 */
public final class ElvaRuntime extends AbstractScriptEngine {
	private final Bindings lisp;

	/**
	 * LISP処理系を構築します。
	 */
	public ElvaRuntime() {
		this(ElvaRuntime.class.getClassLoader());
	}

	/**
	 * LISP処理系を構築します。
	 *
	 * @param loader 関数を供給するクラスローダ
	 */
	public ElvaRuntime(ClassLoader loader) {
		this.lisp = new Forms(new Types(loader));
	}

	/**
	 * LISP処理系に関連付けられる新たなファクトリを返します。
	 *
	 * @return ファクトリ
	 */
	@Override
	public ElvaFactory getFactory() {
		return new ElvaFactory();
	}

	/**
	 * LISP処理系が内蔵する関数や値を参照する環境を返します。
	 *
	 * @return 環境
	 */
	@Override
	public final Bindings createBindings() {
		return new Local(null);
	}

	/**
	 * 指定された入力からLISPの式を読み取ります。
	 *
	 * @param reader 式を読み取るリーダ
	 * @return LISPの式を読み取った結果
	 *
	 * @throws ScriptException 式の構文上の例外
	 */
	public static final ElvaList scan(Reader reader) throws ScriptException {
		try(BufferedReader br = new BufferedReader(reader)) {
			return scan(br.lines().collect(Collectors.joining("\n")));
		} catch (IOException ex) {
			throw new ScriptException(ex);
		}
	}

	/**
	 * 指定された入力からLISPの式を読み取ります。
	 *
	 * @param source 式
	 * @return LISPの式を読み取った結果
	 *
	 * @throws ScriptException 式の構文上の例外
	 */
	public static final ElvaList scan(String source) throws ScriptException {
		try {
			final var list = new ArrayList<ElvaNode>();
			final var scan = new ElvaScanner(source);
			while(scan.hasNext()) list.add(scan.next());
			return ElvaList.chain(list);
		} catch (IOException | ElvaLexicalException ex) {
			throw new ScriptException(ex.getMessage());
		}
	}

	/**
	 * 新たに構築した環境で指定された入力からLISPの式を読み取って評価します。
	 *
	 * @param r 式を読み取るリーダ
	 * @param c 文脈
	 * @return LISPの式を評価した結果
	 *
	 * @throws ScriptException 読み取り、式の構文上または実行時に発生した例外
	 */
	@Override
	public Object eval(Reader r, ScriptContext c) throws ScriptException {
		final Local root = new Local(lisp).merge(c.getBindings(GLOBAL_SCOPE));
		final Local self = new Local(root).merge(c.getBindings(ENGINE_SCOPE));
		final ElvaEval eval = new ElvaEval(self);
		try {
			final var ret = scan(r).map(eval).last();
			c.getBindings(ENGINE_SCOPE).putAll(self);
			return ret.value();
		} catch (ElvaRuntimeException ex) {
			throw ex.toScriptException();
		}
	}

	/**
	 * 新たに構築した環境で指定された入力からLISPの式を読み取って評価します。
	 *
	 * @param s 式
	 * @param c 文脈
	 * @return LISPの最後の式の値
	 *
	 * @throws ScriptException 式の構文上または実行時に発生した例外
	 */
	@Override
	public Object eval(String s, ScriptContext c) throws ScriptException {
		return eval(new StringReader(s), c);
	}
}
