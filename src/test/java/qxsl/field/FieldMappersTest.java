/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link FieldMappers}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/29
 *
 */
public final class FieldMappersTest extends junit.framework.TestCase {
	private final FieldMappers fields = new FieldMappers();
	@Test
	public void testIterator() {
		assertThat(fields.iterator()).isNotNull();
		assertThat(fields.iterator()).hasNext();
	}
}
