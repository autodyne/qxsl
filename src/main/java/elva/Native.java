/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * LISP処理系のシステム関数であることを示す注釈型です。
 *
 *
 * @author Journal of Hamradio Informatics
 *
 * @since 2019/07/02
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Native {
	/**
	 * このシステム関数の名前です。
	 *
	 * @return 関数の名前
	 */
	public String value();
}
