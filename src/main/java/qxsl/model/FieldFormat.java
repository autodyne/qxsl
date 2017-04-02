/*****************************************************************************
 * Amateur Radio Operational Logging Library 'xsum' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import javax.xml.namespace.QName;

/**
 * 交信記録シートにおいて各属性の入出力を実装します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2015/08/05
 * 
 */
public interface FieldFormat {
	/**
	 * この属性の名前を返します。
	 * 
	 * @return 属性の名前
	 */
	public QName type();

	/**
	 * 文字列から{@link Field}のインスタンスを構築します。
	 * 
	 * @param value 属性値を表す文字列
	 * @return 生成された属性値
	 * @throws Exception 値に誤りがある場合に発生する例外
	 */
	public Field decode(String value) throws Exception;

	/**
	 * 指定された属性値を文字列に変換して永続化します。
	 * 
	 * @param field 永続化する属性値
	 * @return 文字列化された属性値
	 */
	public String encode(Field field);
}
