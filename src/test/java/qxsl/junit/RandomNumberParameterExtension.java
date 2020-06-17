/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.junit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Parameter;
import java.security.SecureRandom;

import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolver;

/**
 * 任意の整数値を擬似乱数で生成してテストメソッドの引数にします。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/17
 */
public final class RandomNumberParameterExtension implements ParameterResolver {
	private static final SecureRandom random = new SecureRandom();

	/**
	 * 指定された最大値までの整数値を生成します。
	 *
	 * @param max 最大値 (除外)
	 * @retur 整数値
	 */
	public static final int randInt(int max) {
		return random.nextInt(max);
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target(ElementType.PARAMETER)
	public static @interface RandomNumber {
		public int value() default Integer.MAX_VALUE;
	}

	@Override
	public boolean supportsParameter(ParameterContext p, ExtensionContext e) {
		return p.isAnnotated(RandomNumber.class);
	}

	@Override
	public Integer resolveParameter(ParameterContext p, ExtensionContext e) {
		return random.nextInt(p.findAnnotation(RandomNumber.class).get().value());
	}
}
