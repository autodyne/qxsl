/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.junit;

import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolver;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.security.SecureRandom;

/**
 * 任意の文字列を擬似乱数で生成してテストメソッドの引数にします。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/17
 */
public final class RandomStringParameterExtension implements ParameterResolver {
	private static final SecureRandom random = new SecureRandom();

	/**
	 * 指定された長さまでの文字列を生成します。
	 *
	 * @param max 最大の長さ
	 * @retur 文字列
	 */
	public static final String alnum(int max) {
		final var text = new char[1 + random.nextInt(max)];
		for(int i = 0; i < text.length; i++) switch(random.nextInt(3)) {
			case 0: text[i] = (char)('0' + random.nextInt(10)); break;
			case 1: text[i] = (char)('A' + random.nextInt(26)); break;
			case 2: text[i] = (char)('a' + random.nextInt(26)); break;
		}
		return new String(text);
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target(ElementType.PARAMETER)
	public static @interface RandomString {
		public int value() default 100;
	}

	@Override
	public boolean supportsParameter(ParameterContext p, ExtensionContext e) {
		return p.isAnnotated(RandomString.class);
	}

	@Override
	public String resolveParameter(ParameterContext p, ExtensionContext e) {
		return alnum(p.findAnnotation(RandomString.class).get().value());
	}
}
