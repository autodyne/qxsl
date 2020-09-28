/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * LISP処理系で使用されるリストやアトムの抽象化です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/29
 */
public abstract class NodeBase implements Iterable<NodeBase> {
	private static final AutoBoxing AUTO = new AutoBoxing();
	private static final NodeBoxing NODE = new NodeBoxing();

	/**
	 * 内容が空のリストを示す特別なインスタンスです。
	 */
	public static final EmptySeq NIL = EmptySeq.NIL;

	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 *
	 * @return 値
	 */
	public abstract Object value();

	/**
	 * この式の実数型の内容を返します。
	 *
	 *
	 * @return 実数
	 */
	public final RealNode real() {
		return ofNode(RealNode.class);
	}

	/**
	 * この式の真偽型の内容を返します。
	 *
	 *
	 * @return 真偽
	 */
	public final boolean bool() {
		return ofNode(BoolNode.class).value();
	}

	/**
	 * この式の識別子型の内容を返します。
	 *
	 *
	 * @return 識別子
	 */
	public final NameNode name() {
		return ofNode(NameNode.class);
	}

	/**
	 * この式の型情報型の内容を返します。
	 *
	 *
	 * @return 型情報
	 */
	public final TypeNode type() {
		return ofNode(TypeNode.class);
	}

	/**
	 * この式の文字列型の内容を返します。
	 *
	 *
	 * @return 文字列
	 */
	public final String text() {
		return ofNode(TextNode.class).value();
	}

	/**
	 * この式の演算子型の内容を返します。
	 *
	 *
	 * @return 演算子
	 */
	public final FormBase form() {
		return ofNode(FormBase.class);
	}

	/**
	 * この式のリスト型の内容を返します。
	 *
	 *
	 * @return リスト
	 */
	public final ListBase list() {
		return ofNode(ListBase.class);
	}

	/**
	 * この式がアトムであるか確認します。
	 *
	 *
	 * @return アトムの場合は真
	 */
	public abstract boolean isAtom();

	/**
	 * この式が空の値であるか確認します。
	 *
	 *
	 * @return 空の値の場合は真
	 */
	public final boolean isNull() {
		return this instanceof NullNode;
	}

	/**
	 * この式が識別子であるか確認します。
	 *
	 *
	 * @return 識別子の場合は真
	 */
	public final boolean isName() {
		return this instanceof NameNode;
	}

	/**
	 * この式がリストであるか確認します。
	 *
	 *
	 * @return リストの場合は真
	 */
	public final boolean isList() {
		return this instanceof ListBase;
	}

	/**
	 * この式の値を文字列に変換します。
	 *
	 *
	 * @return 文字列 値がnullの場合はnull
	 */
	public final String valueAsString() {
		return isNull()? null: value().toString();
	}

	/**
	 * 指定された値を{@link NodeBase}で包みます。
	 *
	 *
	 * @param sexp 値
	 *
	 * @return 値
	 */
	public static final NodeBase wrap(Object sexp) {
		return NODE.encode(sexp);
	}

	/**
	 * この式を演算子として引数に適用する式を返します。
	 *
	 *
	 * @param args 被演算子
	 *
	 * @return 演算子及び被演算子のリスト
	 */
	public final ListBase form(Object... args) {
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
		private final List<TypeRule> list;

		private NodeBoxing() {
			this.list = new ArrayList<>();
			list.add(new NodeBase.TYPE());
			list.add(new TypeNode.TYPE());
			list.add(new TextNode.TEXT());
			list.add(new RealNode.REAL());
			list.add(new BoolNode.BOOL());
			list.add(new ArraySeq.LIST());
			list.add(new CoverSeq.LIST());
			list.add(new NullNode.NULL());
		}

		/**
		 * 処理系の外部の値を適切に型変換します。
		 *
		 *
		 * @param value 値
		 *
		 * @return 式
		 */
		private final NodeBase encode(Object value) {
			for(var imp: list) if(imp.support(value)) {
				return imp.encode(value);
			}
			return new AtomNode(value);
		}
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/07/31
	 */
	public static final class TYPE implements TypeRule {
		@Override
		public final boolean support(Object value) {
			return value instanceof NodeBase;
		}

		@Override
		public final NodeBase encode(Object value) {
			return (NodeBase) value;
		}
	}

	/**
	 * この式の内容が指定された型である場合に式の内容を返します。
	 *
	 *
	 * @param type 型
	 *
	 * @return 式の値
	 *
	 * @param <V> 返り値の総称型
	 */
	@SuppressWarnings("unchecked")
	public final <V> V ofType(Class<V> type) {
		final boolean list = type.isArray();
		if(!list) return (V) AUTO.encode(type).cast(value());
		else return (V) list().cast(type.getComponentType());
	}

	/**
	 * この式の実装が指定された型である場合に式の実装を返します。
	 *
	 *
	 * @param type 型
	 *
	 * @return 式の実装
	 *
	 * @param <V> 返り値の総称型
	 */
	public final <V extends NodeBase> V ofNode(Class<V> type) {
		if(type.isInstance(this)) return type.cast(this);
		final var self = this.getClass();
		final var temp = "%s (%s) is detected but %s required";
		final var text = String.format(temp, this, self, type);
		throw new ClassCastException(text);
	}
}
