/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import elva.warn.ElvaRuntimeException;

/**
 * 組込関数として利用される特殊形式の共通実装です。
 * 特殊形式の名前と引数の個数は注釈型で指定します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/07/27
 */
public abstract class NativeOp extends FormBase {
	private final Args args;
	private final Name name;

	/**
	 * 注釈を読み込んで特殊形式を初期化します。
	 *
	 *
	 * @throws ElvaRuntimeException 注釈が不備の場合
	 */
	public NativeOp() {
		this.args = getArgs();
		this.name = getName();
	}

	/**
	 * この演算子の実体を返します。
	 *
	 *
	 * @return 演算子の実体
	 */
	@Override
	public final NativeOp value() {
		return this;
	}

	/**
	 * 特殊形式の引数の個数を指定する注釈型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/17
	 */
	@Target(ElementType.TYPE)
	@Retention(RetentionPolicy.RUNTIME)
	public static @interface Args {
		/**
		 * 評価前の引数の最小限の個数です。
		 *
		 *
		 * @return 引数の個数
		 */
		public int min();

		/**
		 * 評価前の引数の最大限の個数です。
		 *
		 *
		 * @return 引数の個数
		 */
		public int max();
	}

	/**
	 * 特殊形式の固有の名前を指定する注釈型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/02
	 */
	@Target(ElementType.TYPE)
	@Retention(RetentionPolicy.RUNTIME)
	public static @interface Name {
		/**
		 * この特殊形式の名前です。
		 *
		 *
		 * @return 特殊形式の名前
		 */
		public String value();
	}

	private static final String ARGS = "Args not annotated";
	private static final String NAME = "Name not annotated";

	/**
	 * この特殊形式の引数に関する注釈を返します。
	 *
	 *
	 * @return 注釈
	 *
	 * @throws ElvaRuntimeException 注釈が不備の場合
	 */
	public final Args getArgs() throws ElvaRuntimeException {
		final var args = getClass().getAnnotation(Args.class);
		if(args == null) throw new ElvaRuntimeException(ARGS);
		return args;
	}

	/**
	 * この特殊形式の名前に関する注釈を返します。
	 *
	 *
	 * @return 注釈
	 *
	 * @throws ElvaRuntimeException 注釈が不備の場合
	 */
	public final Name getName() throws ElvaRuntimeException {
		final var name = getClass().getAnnotation(Name.class);
		if(name == null) throw new ElvaRuntimeException(NAME);
		return name;
	}

	/**
	 * この特殊形式が可変長引数の特殊形式か確認します。
	 *
	 *
	 * @return 常に偽
	 */
	@Override
	public final boolean isVarArgs() {
		return false;
	}

	/**
	 * この特殊形式が取る引数の最小の個数を返します。
	 *
	 *
	 * @return 最小限の引数の個数
	 */
	@Override
	public final int getMinimumArgumentLength() {
		return args.min();
	}

	/**
	 * この特殊形式が取る引数の最大の個数を返します。
	 *
	 *
	 * @return 最大限の引数の個数
	 */
	@Override
	public final int getMaximumArgumentLength() {
		return args.max() < 0? MAX: args.max();
	}

	/**
	 * この特殊形式を表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return name.value();
	}
}
