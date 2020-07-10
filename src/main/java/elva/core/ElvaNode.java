/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import elva.core.ElvaList.ArraySeq;
import elva.core.ElvaList.ChainSeq;
import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系で使用されるリストやアトムの抽象化です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/29
 */
public abstract class ElvaNode implements Serializable {
	private static final AutoBoxing AUTO = new AutoBoxing();
	private static final NodeBoxing NODE = new NodeBoxing();

	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 * @return 値
	 */
	public abstract Object value();

	/**
	 * この式の整数型の内容を返します。
	 *
	 * @return 整数
	 */
	public final int toInt() {
		return real().intValueExact();
	}

	/**
	 * この式の実数型の内容を返します。
	 *
	 * @return 実数
	 */
	public final BigDecimal real() {
		return ofNode(ElvaReal.class).toBigDecimal();
	}

	/**
	 * この式の真偽型の内容を返します。
	 *
	 * @return 真偽
	 */
	public final boolean bool() {
		return ofNode(ElvaBool.class).value();
	}

	/**
	 * この式の識別子型の内容を返します。
	 *
	 * @return 識別子
	 */
	public final ElvaName name() {
		return ofNode(ElvaName.class).value();
	}

	/**
	 * この式の型情報型の内容を返します。
	 *
	 * @return 型情報
	 */
	public final Class<?> type() {
		return ofNode(ElvaType.class).value();
	}

	/**
	 * この式の文字列型の内容を返します。
	 *
	 * @return 文字列
	 */
	public final String text() {
		return ofNode(ElvaText.class).value();
	}

	/**
	 * この式の演算子型の内容を返します。
	 *
	 * @return 演算子
	 */
	public final ElvaForm form() {
		return ofNode(ElvaForm.class);
	}

	/**
	 * この式の反復処理可能型の内容を返します。
	 *
	 * @return 反復処理可能な値
	 */
	public final Iterable<?> iter() {
		return ofType(Iterable.class);
	}

	/**
	 * この式のリスト型の内容を返します。
	 *
	 * @return リスト
	 */
	public final ElvaList list() {
		return ofNode(ElvaList.class);
	}

	/**
	 * この式の内容が指定された型である場合に式の内容を返します。
	 *
	 * @param type 型
	 * @return 式の値
	 *
	 * @param <V> 返り値の総称型
	 */
	@SuppressWarnings("unchecked")
	public final <V> V ofType(Class<V> type) {
		final var body = value();
		try {
			return (V) AUTO.encode(type).cast(body);
		} catch (ClassCastException ex) {
			final var self = body.getClass();
			final var temp = "%s (%s) is detected but %s required";
			final var text = String.format(temp, body, self, type);
			throw new ClassCastException(text);
		}
	}

	/**
	 * この式の実装が指定された型である場合に式の実装を返します。
	 *
	 * @param type 型
	 * @return 式の実装
	 *
	 * @param <V> 返り値の総称型
	 */
	public final <V extends ElvaNode> V ofNode(Class<V> type) {
		try {
			return type.cast(this);
		} catch (ClassCastException ex) {
			final var self = this.getClass();
			final var temp = "%s (%s) is detected but %s required";
			final var text = String.format(temp, this, self, type);
			throw new ClassCastException(text);
		}
	}

	/**
	 * 指定された値を{@link ElvaNode}で包みます。
	 *
	 * @param sexp 値
	 * @return 値
	 */
	public static final ElvaNode wrap(Object sexp) {
		if(sexp instanceof ElvaNode) return (ElvaNode) sexp;
		return NODE.encode(sexp);
	}

	/**
	 * この式を演算子として引数に適用する式を返します。
	 *
	 * @param args 被演算子
	 * @return 演算子及び被演算子のリスト
	 */
	public final ElvaList form(Object...args) {
		return new ChainSeq(this, new ArraySeq(args));
	}

	/**
	 * 基本型とそのラッパ型との変換を実施します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/08
	 */
	private static final class AutoBoxing {
		private final Map<Class<?>, Class<?>> encode;
		private final Map<Class<?>, Class<?>> decode;

		private AutoBoxing() {
			this.encode = new HashMap<>();
			this.decode = new HashMap<>();
			install(Long.class);
			install(Byte.class);
			install(Short.class);
			install(Float.class);
			install(Double.class);
			install(Integer.class);
			install(Boolean.class);
			install(Character.class);
		}

		/**
		 * 指定されたクラスをラッパ型として登録します。
		 *
		 * @param type ラッパ型
		 */
		private final void install(Class<?> type) {
			try {
				final var fld = type.getField("TYPE");
				final var cls = (Class) fld.get(null);
				encode.put(cls, type);
				decode.put(type, cls);
			} catch (NoSuchFieldException ex) {
			} catch (IllegalAccessException ex) {}
		}

		/**
		 * 指定された基本型に適合するラッパ型を返します。
		 *
		 * @param cls 型
		 *
		 * @return 適合するラッパ型
		 */
		private final Class<?> encode(Class<?> cls) {
			return encode.getOrDefault(cls, cls);
		}
	
		/**
		 * 指定されたラッパ型に適合する基本型を返します。
		 *
		 * @param cls 型
		 *
		 * @return 適合する基本型
		 */
		private final Class<?> decode(Class<?> cls) {
			return decode.getOrDefault(cls, cls);
		}
	}

	/**
	 * 処理系の内外における暗黙的な型変換を実施します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/09
	 */
	private static final class NodeBoxing {
		private final List<Implicit> list;

		private NodeBoxing() {
			this.list = new ArrayList<>();
			list.add(new ElvaType.TYPE());
			list.add(new ElvaText.TEXT());
			list.add(new ElvaReal.REAL());
			list.add(new ElvaBool.BOOL());
			list.add(new ElvaList.LIST());
			list.add(new JavaList.LIST());
		}

		/**
		 * 処理系の外部の値を適切に型変換します。
		 *
		 * @param value 値
		 * @return 式
		 */
		private final ElvaNode encode(Object value) {
			for(var imp: list) if(imp.support(value)) {
				return imp.encode(value);
			}
			return new ElvaWrap(value);
		}
	}
}
