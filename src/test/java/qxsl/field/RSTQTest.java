/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import java.util.Random;
import org.junit.Test;
import qxsl.model.Fields;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;
import static qxsl.table.secret.QxmlFormat.RSTQ;

/**
 * {@see RSTQ}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class RSTQTest extends junit.framework.TestCase {
	private final Fields fields = new Fields(RSTQ);
	private final Random random = new Random();
	@Test
	public void testValue() {
		final int rst = random.nextInt(489) + 111;
		final int exp = rst % 10 == 0? rst / 10 : rst;
		assertThat(new RSTQ(rst).value(), is(exp));
	}
	@Test
	public void testRSTQ$Format() throws Exception {
		final RSTQ.Format $form = new RSTQ.Format();
		final RSTQ rstq = new RSTQ(random.nextInt(489) + 111);
		assertThat($form.decode($form.encode(rstq)), is(rstq));
		assertThat(fields.cache($form.encode(rstq)), is(rstq));
	}
}
