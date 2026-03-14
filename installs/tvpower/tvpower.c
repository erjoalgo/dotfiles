
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/module.h>
#include <linux/sysrq.h>

MODULE_DESCRIPTION("Power on TVs via SysRq");
MODULE_AUTHOR("Ernesto"); // based on https://snee.la/posts/sysrq/
MODULE_LICENSE("GPL");


#define MOD_PREFIX "[TV Power] "
#define TVPOWER_KEY_STRING "P"
#define TVPOWER_KEY (TVPOWER_KEY_STRING[0])
#define TVPOWER_HELP "power on TVs(" TVPOWER_KEY_STRING ")"

static int is_sysrq_registered = 0;


static void sysrq_handle_power_on_tvs(int key)
{
    // Don't use pr_emerg unless it's an emergency. In our case, drinking
    // coffee is an emergency.
    pr_emerg(MOD_PREFIX "The kernel suggests you drink some coffee\n");
    pr_emerg(MOD_PREFIX "P.S.: Don't drink tea, it's caca\n");
    pr_emerg(MOD_PREFIX "now running tv power...");
    pr_emerg(MOD_PREFIX "done running tv power...");
}


static const struct sysrq_key_op sysrq_power_on_tvs_op = {
	.handler	= sysrq_handle_power_on_tvs,
	.help_msg	= TVPOWER_HELP,
	.action_msg	= "Power on TVs in the Garage"
};


static int power_on_tvs_sysrq_init(void)
{
    pr_debug(MOD_PREFIX " Initializing\n");

#ifdef CONFIG_MAGIC_SYSRQ
    is_sysrq_registered = !register_sysrq_key(TVPOWER_KEY, &sysrq_power_on_tvs_op);
    pr_debug(MOD_PREFIX "SysRq Registered: %d\n", is_sysrq_registered);
#endif

    return 0;
}

static void power_on_tvs_sysrq_exit(void)
{
#ifdef CONFIG_MAGIC_SYSRQ
    pr_debug(MOD_PREFIX "Unregistering SysRq\n");

    if (is_sysrq_registered)
        unregister_sysrq_key(TVPOWER_KEY, &sysrq_power_on_tvs_op);
#endif

    pr_debug(MOD_PREFIX "Completed!\n");
}


module_init(power_on_tvs_sysrq_init);
module_exit(power_on_tvs_sysrq_exit);
