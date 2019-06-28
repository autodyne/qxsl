/*****************************************************************************
 * Amateur Radio Operational Logging Library 'xsum' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.model.Item;
import qxsl.model.Rcvd;
import qxsl.model.Sent;
import qxsl.model.Tuple;

/**
 * {@link Field}を別の名前空間の属性に変換する仕組みを提供します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/28
 * 
 */
public interface FieldMapper {
	/**
	 * 変換先の属性の名前を返します。
	 * 
	 * @return 属性の名前
	 */
	public QName target();

	/**
	 * 指定された要素から対象の属性を検索します。
	 * 属性が見つかると、変換後の属性を返します。
	 *
	 * @param item 属性を検索する要素
	 * @return 変換後の属性 見つからない場合はnull
	 */
	default Field search(Item item) {
		return null;
	}

	/**
	 * 指定された要素から対象の属性を検索します。
	 * 属性が見つかると、変換後の属性を返します。
	 *
	 * @param rcvd 属性を検索する要素
	 * @return 変換後の属性 見つからない場合はnull
	 */
	default Field search(Rcvd rcvd) {
		return null;
	}

	/**
	 * 指定された要素から対象の属性を検索します。
	 * 属性が見つかると、変換後の属性を返します。
	 *
	 * @param sent 属性を検索する要素
	 * @return 変換後の属性 見つからない場合はnull
	 */
	default Field search(Sent sent) {
		return null;
	}

	/**
	 * 指定された要素から対象の属性を検索します。
	 * 属性が見つかると、変換後の属性を返します。
	 *
	 * @param tuple 属性を検索する要素
	 * @return 変換後の属性 見つからない場合はnull
	 */
	default Field search(Tuple tuple) {
		if(tuple instanceof Item) return search((Item) tuple);
		if(tuple instanceof Rcvd) return search((Rcvd) tuple);
		if(tuple instanceof Sent) return search((Sent) tuple);
		return null;
	}
}
